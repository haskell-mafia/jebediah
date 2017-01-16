{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jebediah.File (
    LineCount (..)
  , LineByteCount (..)
  , withFileSyncer
  , closeFileSyncer
  , startFileSync
  , bracketFileSync
  , fsPut
  , fsPutNoTime
  , incrementFileSyncBytes
  , incrementFileSyncByteCount
  , prependTimeToLine
  , timedLineToLog
  , readFileLines
  ) where

import qualified Control.Concurrent.Async as A
import           Control.Monad.Catch (bracket, handleIf)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Unsafe as BU
import           Data.Char (ord)
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (defaultTimeLocale, getCurrentTime, formatTime, parseTimeM)
import           Data.Word (Word64)

import           Jebediah.Data
import qualified Jebediah.Conduit as J

import           Mismi (Env)

import           P

import           System.FilePath (FilePath)
import           System.IO (IO)
import           System.IO.Error (isEOFError)
import qualified System.Posix.IO as Posix
import qualified System.Posix.Types as Posix
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixBS
import           System.Posix.Types (ByteCount, Fd)

import           Twine.Snooze (snooze, milliseconds)

newtype LineCount =
  LineCount {
      lineCount :: Word64
    } deriving (Eq, Num, Show)

newtype LineByteCount =
  LineByteCount {
      lineByteCount :: Word64
    } deriving (Eq, Num, Ord, Show)

data FileSyncer =
  FileSyncer Fd (IORef.IORef (Bool, ByteCount))

withFileSyncer :: FilePath -> (FileSyncer -> IO a) -> IO a
withFileSyncer file f = do
  withBinaryFile
    file
    Posix.ReadWrite
    (Just 0o666)
    -- Make sure the file exists before we start reading
    Posix.defaultFileFlags { Posix.trunc = True }
    $ \fd -> do
      fs <- FileSyncer fd <$> IORef.newIORef (False, 0)
      f fs

closeFileSyncer :: FileSyncer -> IO ()
closeFileSyncer (FileSyncer _ v) =
  IORef.atomicModifyIORef' v $ \(_, b) -> ((True, b), ())

-- | Starts an upload to cloudwatch by streaming from a buffering file.
--
-- This function _will_ block and should be used with "Async",
-- and should normally be used with "bracketFileSync"
startFileSync ::
  Env ->
  LogGroup ->
  LogStream ->
  ExclusiveSequence ->
  ByteCount ->
  LineByteCount ->
  FileSyncer ->
  IO LineCount
startFileSync env group stream next bufferSize lineSize fileSyncer =
  J.sinkBracketOut env group stream next $ \sink' ->
    readFileLines
      fileSyncer
      bufferSize
      lineSize
      (\bs -> timedLineToLogOrNow bs >>= sink' . J.cleanLog)

-- | A safe way to consume "startFileSync" so that it runs in the background
bracketFileSync :: FileSyncer -> IO b -> IO a -> IO a
bracketFileSync fs start f =
  A.withAsync start $ \a -> do
    x <- f
    closeFileSyncer fs
    void $ A.wait a
    pure x

timeFormat :: [Char]
timeFormat =
  "%Y-%m-%d %H:%M:%S"

timeFormatLength :: Int
timeFormatLength =
  19

prependTimeToLine :: MonadIO m => ByteString -> m ByteString
prependTimeToLine bs =
  flip fmap (liftIO getCurrentTime) $ \t ->
    (T.encodeUtf8 . T.pack . formatTime defaultTimeLocale timeFormat) t <> bs

-- | Always produce a log for a line, hopefully with the generated time but otherwise fallback to now
timedLineToLogOrNow :: ByteString -> IO Log
timedLineToLogOrNow bs =
  maybe (Log (T.decodeUtf8 bs) <$> getCurrentTime) pure . timedLineToLog $ bs

timedLineToLog :: ByteString -> Maybe Log
timedLineToLog bs =
  case T.splitAt timeFormatLength . T.decodeUtf8 $ bs of
    (dt, r) ->
      Log r <$> parseTimeM True defaultTimeLocale timeFormat (T.unpack dt)

-- Append bytes to a "FileSyncer" for uploading to Cloudwatch
--
-- To be used with "startFileSync".
--
fsPut :: FileSyncer -> ByteString -> IO ()
fsPut fs =
  prependTimeToLine >=> fsPutNoTime fs

-- This will append a raw line without a timestamp, which will be uploaded to cloudwatch with
-- an incorrect timestamp
--
-- WARNING: For testing only
--
fsPutNoTime :: FileSyncer -> ByteString -> IO ()
fsPutNoTime fs@(FileSyncer h _) bs = do
  void $ PosixBS.fdWrite h bs
  incrementFileSyncBytes fs bs

incrementFileSyncBytes :: FileSyncer -> ByteString -> IO ()
incrementFileSyncBytes fs =
  incrementFileSyncByteCount fs . fromIntegral . BS.length

incrementFileSyncByteCount :: FileSyncer -> ByteCount -> IO ()
incrementFileSyncByteCount (FileSyncer _ v) b =
  IORef.atomicModifyIORef' v $ \(d, c) ->
    let !x = b + c in ((d, x), ())

-- | Sinks a single line of a file at a time until the number of bytes read matches the expected
-- value, which is being updated by a competing consumer
readFileLines ::
  FileSyncer ->
  ByteCount ->
  LineByteCount ->
  (ByteString -> IO ()) ->
  IO LineCount
readFileLines (FileSyncer h expectedBytes) bufferSize lineSize write =
  let
    newline = fromIntegral . ord $ '\n'
    wait n =
      snooze . milliseconds $ min 1000 (10 ^ n)
    write' =
      write . Lazy.toStrict . BSB.toLazyByteString
    flushEnd !n !p !i !l !bs = do
      eb' <- IORef.readIORef expectedBytes
      case eb' of
        (False, _) -> do
          when (n > 0) $
            wait n
          go (n + 1) p i l bs
        (True, eb) ->
          if p < fromIntegral eb then do
            wait (n + 1)
            go (n + 1) p i l bs
          else
            if l == 0 then
              pure i
            else do
              write' bs
              pure $ i + 1
    go !n !p !i !l !bd = do
      bs <- fdGetSomeAt h bufferSize (Posix.COff p)
      go' n p i l bd bs
    go' !n !p !i !l !bd bs =
      case BS.null bs of
        True ->
          flushEnd 0 p i l bd
        False ->
          case BS.elemIndex newline bs of
            Nothing ->
              if fromIntegral (l + BS.length bs) > lineSize then do
                -- NOTE: The lines are too long, we will split the line at this point
                let
                  np =
                    fromIntegral (lineByteCount lineSize) - l
                write' $ bd <> BSB.byteString (BU.unsafeTake np bs)
                go'
                  0
                  (p + fromIntegral np)
                  (i + 1)
                  0
                  mempty
                  (BU.unsafeDrop np bs)
              else do
                go
                  n
                  (p + fromIntegral (BS.length bs))
                  i
                  (l + BS.length bs)
                  (bd <> BSB.byteString bs)
            Just np -> do
              write' $ bd <> BSB.byteString (BU.unsafeTake np bs)
              go'
                0
                (p + fromIntegral np + 1)
                (i + 1)
                0
                mempty
                (BU.unsafeDrop (np + 1) bs)
  in
    go (0 :: Int) 0 0 0 mempty

------------------------

fdGetSomeAt :: Fd -> ByteCount -> Posix.FileOffset -> IO ByteString
fdGetSomeAt fd i fo =
  handleIf isEOFError (const $ pure "") $
    PosixBS.fdPread fd i fo

withBinaryFile :: FilePath -> Posix.OpenMode -> Maybe Posix.FileMode -> Posix.OpenFileFlags -> (Fd -> IO a) -> IO a
withBinaryFile path mode fileMode flags =
  bracket
    (Posix.openFd path mode fileMode flags)
    Posix.closeFd
