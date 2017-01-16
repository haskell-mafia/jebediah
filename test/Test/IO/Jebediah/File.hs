{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Jebediah.File where

import           Control.Monad.Catch (bracket_)

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit.List as C
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Core.IO (testIO)

import           Jebediah.Data
import qualified Jebediah.Conduit as J
import           Jebediah.File
import           Jebediah.Structure

import           Mismi (rawRunAWS)

import           P

import           System.IO (IO)
import           System.FilePath (FilePath, (</>))
import           System.IO.Temp (withTempDirectory)

import           Test.IO.Jebediah.Conduit
import           Test.QuickCheck (Gen, (===), conjoin, forAllProperties, maxSuccess, quickCheckWithResult, stdArgs)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Instances ()

prop_sink_file_lines_before =
  QC.forAll genLinesBytes $ \lines ->
  testIO $
    withFileSyncer' $ \fs -> do
      mLines <- IORef.newIORef []
      for_ lines $ fsPutNoTime fs . flip (<>) "\n"
      closeFileSyncer fs
      r <- readFileLines fs 1024 2048 (\b -> () <$ IORef.modifyIORef mLines ((:) b))
      lines' <- reverse <$> IORef.readIORef mLines
      pure . conjoin $ [
          lines' === lines
        , r === (LineCount . fromIntegral . length) lines
        ]

prop_sink_file_lines_after =
  QC.forAll genLinesBytes $ \lines ->
  testIO $
    withFileSyncer' $ \fs -> do
      mLines <- IORef.newIORef []
      bracketFileSync fs
        (readFileLines fs 1024 2048 (\b -> () <$ IORef.modifyIORef mLines ((:) b)))
        $
          for_ lines $ fsPutNoTime fs . flip (<>) "\n"
      lines' <- reverse <$> IORef.readIORef mLines
      pure . conjoin $ [
          lines' === lines
        ]

prop_sink_file_lines_small_buffer =
  QC.forAll (QC.choose (1, 1024)) $ \j ->
  QC.forAll (QC.choose (1, 1024)) $ \i ->
  QC.forAll (QC.choose (i + 1, 20)) $ \n ->
  QC.forAll (T.encodeUtf8 . T.pack <$> QC.vectorOf n (QC.choose ('a', 'z'))) $ \line ->
  testIO $
    withFileSyncer' $ \fs -> do
      mLines <- IORef.newIORef []
      fsPutNoTime fs line
      closeFileSyncer fs
      r <- readFileLines fs j (fromIntegral i) (\b -> () <$ IORef.modifyIORef mLines ((:) b))
      lines' <- reverse <$> IORef.readIORef mLines
      let
        lines = splitInterval i line
      pure . conjoin $ [
          lines' === lines
        , r === (LineCount . fromIntegral . length) lines
        ]

prop_file_log_time =
  QC.forAll genLine $ \line ->
    testIO $ do
      tl <- prependTimeToLine . T.encodeUtf8 $ line
      pure $ (fmap logChunk . timedLineToLog) tl === Just line

prop_file_log_time_fail =
  QC.forAll genLine $ \line ->
    timedLineToLog (T.encodeUtf8 line) === Nothing

prop_file_sync group stream =
  QC.forAll genLines $ \lines ->
  testIO $ do
    env <- testenv
    withFileSyncer' $ \fs ->
      bracket_
        (rawRunAWS env $ newLogGroup group >> newLogStream group stream)
        (deleteLogStream env group stream) $ do
          lock <- newExclusiveSequence Nothing
          for_ lines $ fsPut fs . flip (<>) "\n" . T.encodeUtf8
          closeFileSyncer fs
          r <- startFileSync env group stream lock 1024 fs
          ls <- J.source env group stream Everything NoFollow =$= J.unclean $$ C.consume
          pure . conjoin $ [
              fmap logChunk ls === lines
            , r === (LineCount . fromIntegral . length) lines
            ]

---------------

withFileSyncer' f =
  withTempFile $ \file ->
    withFileSyncer file f

withTempFile :: (FilePath -> IO a) -> IO a
withTempFile f =
  withTempDirectory "dist" "sink_file" $ \dir -> do
     f (dir </> "test")

genLine :: Gen Text
genLine =
  T.pack <$> QC.listOf (QC.choose ('a', 'z'))

genLines :: Gen [Text]
genLines =
  QC.listOf1 genLine

genLinesBytes :: Gen [ByteString]
genLinesBytes =
  fmap T.encodeUtf8 <$> genLines

splitInterval :: Int -> ByteString -> [ByteString]
splitInterval n bs =
  let
    (a, b) = BS.splitAt n bs
  in
    (:) a $
      if BS.null b then
        []
      else
        splitInterval n b

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
