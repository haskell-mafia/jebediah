{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Conduit (
    source
  , clean
  , unclean
  , sink
  , sinkBracket
  , sourceFileLines
  , sourceHandleLines
  ) where


import qualified Control.Concurrent.Async as A
import           Control.Concurrent.MVar (modifyMVar_, readMVar)
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.STM.TBChan as S
import           Control.Lens ((^.))
import           Control.Monad.Catch (bracket)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadResource)

import           Data.Conduit (Conduit, Source, Sink)
import qualified Data.Conduit as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           Jebediah.Data
import           Jebediah.Single

import           Mismi (rawRunAWS)
import           Mismi.Amazonka (Env)
import qualified Mismi.CloudwatchLogs.Amazonka as M

import           P

import           System.FilePath (FilePath)
import           System.IO (IO, IOMode (..), Handle, openFile, hClose)
import           System.IO.Error (IOError, tryIOError)

import           Twine.Data.Pin (Pin, newPin, checkPin, pullPin)
import           Twine.Snooze (snooze, milliseconds, seconds)

import           X.Control.Monad.Trans.Either (EitherT, left)

-- |
-- Cloudwatch Logs doesn't accept empty messages so it is painful to represent
-- empty lines.
--
-- 'clean' provides a standard transformation to warp empty messages, and can
-- be undone with 'unclean'.
--
clean :: Monad m => Conduit Log m Log
clean =
  C.awaitForever $ \(Log text time) ->
    C.yield $ Log (if T.null text then "." else text) time

-- |
-- Cloudwatch Logs doesn't accept empty messages so it is painful to represent
-- empty lines.
--
-- 'unclean' provides a standard transformation to reverse the warping of empty
-- messages that have been transformed by 'clean'
--
unclean :: Monad m => Conduit Log m Log
unclean =
  C.awaitForever $ \(Log text time) ->
    C.yield $ Log (if text == "." then "" else text) time

-- |
-- A source for accessing an existing log-stream, this will return all available
-- log events within the bounds of 'Query'.
--
-- 'Follow' can be used to extend the source to wait for new events once the
-- stream is empty.
--
-- 'NoFollow' implies that we stop once we reach the current end of
-- the stream (pagination still applies up until that point).
--
source :: Env -> LogGroup -> LogStream -> Query -> Following -> Source IO Log
source env group stream query following = do
  y <- liftIO . rawRunAWS env $ read group stream query
  for_ (y ^. M.glersEvents) $ \e ->
    case (e ^. M.oleMessage, e ^. M.oleTimestamp) of
      (Just text, Just time) ->
        C.yield $ Log text (unixToUtc time)
      _ ->
        pure ()
  case (y ^. M.glersNextForwardToken) of
    Nothing ->
      pure ()
    Just next -> do
      case (y ^. M.glersEvents, following) of
        ([], NoFollow) ->
          pure ()
        ([], Follow wait) -> do
          liftIO $ snooze wait
          source env group stream (At . Sequence $ next) following
        (_, _)   -> do
          source env group stream (At . Sequence $ next) following

data SinkState =
  SinkState !(S.TBChan Log) !(A.Async ()) !Pin

aquireSinkState :: Env -> LogGroup -> LogStream -> ExclusiveSequence -> IO SinkState
aquireSinkState env group stream next = do
  chan <- liftIO $ S.atomically (S.newTBChan 10000)
  p <- newPin
  consumer <- liftIO . A.async $ consume env group stream next chan p
  pure $ SinkState chan consumer p

releaseSinkState :: Env -> LogGroup -> LogStream -> ExclusiveSequence -> SinkState -> IO ()
releaseSinkState env group stream next (SinkState chan consumer p) = do
  pullPin p
  A.wait consumer
  complete env group stream next chan

-- |
-- Bracket style safe sink to ensure everything is flushed upon completion.
-- Useful for situations where you want to re-use the sink, or use in parallel
-- and want better control over how many async's are running and making request.
--
-- This differs from sync that use a consumer per fuse of the sink, in that only
-- one async is used no mater how many times you "sink".
--
sinkBracket :: Env -> LogGroup -> LogStream -> ExclusiveSequence -> (Sink Log IO (Maybe Sequence) -> IO a) -> IO a
sinkBracket env group stream next f = do
  bracket
    (aquireSinkState env group stream next)
    (releaseSinkState env group stream next)
    (f . sink' env group stream next)

-- |
-- Resource safe sink using ResourceT to ensure everything is flushed upon completion.
--
sink :: MonadResource m => Env -> LogGroup -> LogStream -> ExclusiveSequence -> Sink Log m (Maybe Sequence)
sink env group stream next =
  C.bracketP
    (aquireSinkState env group stream next)
    (releaseSinkState env group stream next)
    (sink' env group stream next)

sink' :: MonadIO m => Env -> LogGroup -> LogStream -> ExclusiveSequence -> SinkState -> Sink Log m (Maybe Sequence)
sink' env group stream next s@(SinkState chan _ _) = do
  line <- C.await
  case line of
    Nothing ->
      liftIO . readMVar . exclusiveSequence $ next
    Just a -> do
      liftIO . S.atomically . S.writeTBChan chan $ a
      sink' env group stream next s

consume :: Env -> LogGroup -> LogStream -> ExclusiveSequence -> S.TBChan Log -> Pin -> IO ()
consume env group stream next logs p = do
  n <- drain env group stream next logs
  when (n < 10) $
    (snooze . seconds $ 1)
  unlessM (checkPin p) $
    consume env group stream next logs p

complete :: Env -> LogGroup -> LogStream -> ExclusiveSequence -> S.TBChan Log -> IO ()
complete env group stream next logs = do
  n <- drain env group stream next logs
  unless (n == 0) $
    complete env group stream next logs

-- |
-- Batch as many events off the queue as we can and upload.
--
-- For the most part we will accumulate until we have caught up with
-- the producer and then send - letting the producer getting ahead
-- again. However, there are a number of additional limits which
-- can also trigger a send even if we are behind:
--
--  - The maximum batch size is 1,048,576 bytes.
--
--  - The maximum number of log events in a batch is 10,000.
--
--  - We don't want to spend too much time before submitting, so
--    every 100 events, we also check for the clock, if we have
--    spent more than 10 seconds doing work we cut off the batch
--    and send.
--
-- Reference: http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html
--
drain :: Env -> LogGroup -> LogStream -> ExclusiveSequence -> S.TBChan Log -> IO Int
drain env group stream next logs = do
  start <- getCurrentTime
  let
    -- handle undocumented limit: Log event too large: 404439 bytes exceeds limit of 262144
    disect :: Log -> [Log]
    disect l@(Log c t) =
      if sizeOf l < 262000
        then [l]
        else let (a, b) = T.splitAt (T.length c `div` 2) c in join $ disect <$> [Log a t, Log b t]

    rechunk :: [a] -> [[a]]
    rechunk [] =
      []
    rechunk xs =
      let (a, b) = splitAt 10000 xs in a : rechunk b

    peek :: IO (Maybe Log)
    peek =
      S.atomically $ S.tryPeekTBChan logs

    acknowledge :: IO ()
    acknowledge =
      void . S.atomically . S.readTBChan $ logs

    overdue :: [Log] -> IO Bool
    overdue acc =
      case length acc `mod` 100 == 0 of
        False ->
          pure False
        True -> do
          check <- getCurrentTime
          pure $ diffUTCTime check start > 10

    overflow :: Int -> [Log] -> Log -> Bool
    overflow size acc x =
      (size + sizeOf x) >= 1048576 || (length acc) >= 10000

    handle :: Int -> [Log] -> Log -> IO [Log]
    handle size acc x =
      ifM (overdue acc)
        (pure $ x : acc)
        (collect (size + sizeOf x) (x : acc))

    collect :: Int -> [Log] -> IO [Log]
    collect size acc =
      peek >>= \x -> case x of
        Nothing -> do
          check <- getCurrentTime
          if diffUTCTime check start > 1
            then pure acc
            else (snooze . milliseconds) 100 >> collect size acc
        Just event | overflow size acc event ->
          pure acc
        Just event ->
          acknowledge >> handle size acc event

  batch <- (L.reverse . fudge) <$> collect 0 []

  for_ (rechunk $ batch >>= disect) $ \b ->
    modifyMVar_ (exclusiveSequence next) $ \token -> do
      next' <- rawRunAWS env $ write group stream token b
      pure $ maybe token Just next'

  pure $ length batch

sourceFileLines :: MonadResource m => FilePath -> Source (EitherT IOError m) Log
sourceFileLines path = do
  C.bracketP
    (openFile path ReadMode)
    (hClose)
    sourceHandleLines

sourceHandleLines :: MonadIO m => Handle -> Source (EitherT IOError m) Log
sourceHandleLines h = do
  a <- liftIO $ tryIOError (T.hGetLine h)
  t <- liftIO getCurrentTime
  case a of
    Right x ->
      C.yield (Log x t) >> sourceHandleLines h
    Left e ->
      lift $ left e
