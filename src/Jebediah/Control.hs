{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Control (
    listLogGroups
  , listLogGroups'
  , createLogGroup
  , listLogStreams
  , listLogStreams'
  , createLogStream
  , retrieveLogStream'
  , logSink
  ) where

import           P hiding (reverse)
import           Control.Concurrent (threadDelay)
import           Control.Lens hiding ((&))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Mismi
import           Mismi.Amazonka hiding (await)
import           Mismi.CloudwatchLogs.Amazonka hiding (createLogGroup, createLogStream)
import qualified Mismi.CloudwatchLogs.Amazonka as MA

import qualified Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           Delorean.Duration

import           Jebediah.Data

import           Units

listLogGroups :: MonadAWS m
              => Maybe GroupName
              -> m [LogGroup]
listLogGroups prefixName
  = liftAWS
  $ flip ($$) DC.consume
  $ listLogGroups' prefixName

listLogGroups' :: Maybe GroupName
               -> Source AWS LogGroup
listLogGroups' prefixName
  = flip (=$=) (DC.concatMap (^. dlgrsLogGroups))
  $ paginate
  $ describeLogGroups
  & dlgLogGroupNamePrefix .~ fmap unGroupName prefixName

createLogGroup :: MonadAWS m
               => GroupName
               -> m ()
createLogGroup
  = liftAWS
  . fmap (const ()) . send
  . MA.createLogGroup
  . unGroupName

listLogStreams :: MonadAWS m
               => GroupName
               -> Maybe StreamName
               -> m [LogStream]
listLogStreams groupName' prefixName
  = liftAWS
  $ flip ($$) DC.consume
  $ listLogStreams' groupName' prefixName

listLogStreams' :: GroupName
                -> Maybe StreamName
                -> Source AWS LogStream
listLogStreams' (GroupName groupName) prefixName
  = flip (=$=) (DC.concatMap (^. dlsrsLogStreams))
  $ paginate
  $ describeLogStreams groupName
  & dlssLogStreamNamePrefix .~ fmap unStreamName prefixName

createLogStream :: MonadAWS m
                => GroupName
                -> StreamName
                -> m ()
createLogStream (GroupName groupName) (StreamName streamName)
  = liftAWS
  . fmap (const ()) . send
  . MA.createLogStream groupName
  $ streamName

-- getLogEvents does *not* implement pagination, so I'm doing it myself here.
retrieveLogStream' :: GroupName
                   -> StreamName
                   -> Maybe UTCTime
                   -> Maybe UTCTime
                   -> Maybe Text
                   -> Following
                   -> Source AWS OutputLogEvent
retrieveLogStream' groupName streamName start end nxt following
 = do
  y <- lift $ retrieveLogStream'' groupName streamName start end nxt
  traverse_ yield (y ^. glersEvents)
  case (y ^. glersNextForwardToken) of
    Nothing     -> pure ()
    Just (nxt') -> do
      case (y ^. glersEvents, following) of
        ([], NoFollow) -> pure ()
        ([], Follow waitTime)   -> do
          -- Pause for 10 seconds before making the next request.
          liftIO (threadDelay (1000000 * waitTime))
          retrieveLogStream' groupName streamName start end (Just nxt') following
        (_, _)   -> do
          retrieveLogStream' groupName streamName start end (Just nxt') following

retrieveLogStream'' :: GroupName
                    -> StreamName
                    -> Maybe UTCTime
                    -> Maybe UTCTime
                    -> Maybe Text
                    -> AWS GetLogEventsResponse
retrieveLogStream'' (GroupName groupName) (StreamName streamName) start end Nothing
 = send
 $ getLogEvents groupName streamName
 & gleStartTime     .~ start'
 & gleEndTime       .~ end'
 & gleStartFromHead .~ (Just True)
  where
    --  A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
    start' = round . (*1000) . utcTimeToPOSIXSeconds <$> start
    end'   = round . (*1000) . utcTimeToPOSIXSeconds <$> end

retrieveLogStream'' (GroupName groupName) (StreamName streamName) _ _ x@(Just _)
 = send
 $ getLogEvents groupName streamName
 & gleNextToken .~ x

-- Conduit sink which takes lines pairs, batches them, and sends them up.
-- Takes care to ensure sequence tokens are used for separate jobs, but will generally be
-- called initially with Nothing for the token parameter.

--  Invariants:  The maximum batch size is 1,048,576 bytes, and this size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
--  * None of the log events in the batch can be more than 2 hours in the future.
--  * None of the log events in the batch can be older than 14 days or the retention period of the log group.
--  * The log events in the batch must be in chronological ordered by their timestamp.
--  * The maximum number of log events in a batch is 10,000.
--  * A batch of log events in a single PutLogEvents request cannot span more than 24 hours. Otherwise, the PutLogEvents operation will fail.

--  Other invariants discovered
--  * All entries must have text in them (empty is not allowed, we do however filter them out here)

--  Operational limitations
--  * A single line can not be greater than 1MB

logSink :: BatchSize
        -> GroupName
        -> StreamName
        -> Maybe Text
        -> Sink (UTCTime, Text) AWS ()
logSink batchSize groupName streamName initialSequenceNumber = buffer =$ logSinkNel groupName streamName initialSequenceNumber
  where
    buffer = do
      a <- await
      case a of
        Nothing -> return ()
        Just a'@(ts,message) ->
          bufferNel 1 (getSize message) ts a' (toDiff [])

    bufferNel num currentSize firstT firstM rest = do
      a <- await
      case a of
        Nothing -> yield (firstM :| fromDiff rest)
        Just a'@(ts,message) | not (T.null message) -> do
          let size     = getSize message
          let newSize  = size + currentSize
          let newNum   = num + 1
          let timeDiff = floor . toRational $ diffUTCTime ts firstT -- eww
          if shouldSend num newSize timeDiff
            then do
              yield (firstM :| fromDiff rest)
              bufferNel 1 size ts a' (toDiff [])
            else
              bufferNel newNum newSize firstT firstM (rest . (toDiff [a']))
        -- The text line is empty, we have to ignore it.
        Just _ -> bufferNel num currentSize firstT firstM rest

    -- As mentioned above, size in bytes in UTF8 + 26.
    getSize message = B.length (encodeUtf8 message) + 26

    -- We send if any of our invariants are passed.
    shouldSend num size time = case batchSize of
      (BatchSize n s t) -> num >= n || fromIntegral size >= toBytes s || time >= durationToSeconds t

      --case a of
      --  Nothing -> return ()
      --  Just a' -> do
      --    as <- replicateM (n - 1) await
      --    yield (a' :| catMaybes as)
      --    buffer

-- Conduit sink which takes in a single NEL and pushes it up.
-- This function takes care to use the next sequence token each
-- time it is run.
logSinkNel :: GroupName
           -> StreamName
           -> Maybe Text
           -> Sink (NonEmpty (UTCTime, Text)) AWS ()
logSinkNel groupName streamName sequenceToken
 = do
  as <- await
  case as of
    Nothing  -> return ()
    Just as' -> do
      res <- lift $ writeLogNel groupName streamName sequenceToken as'
      logSinkNel groupName streamName (res ^. plersNextSequenceToken)

-- Write a batch to a log stream without any checking of invariants.
-- Sequence tokens, the size of the log sections, it's all up for grabs.
writeLogNel :: MonadAWS m
            => GroupName                -- Log group
            -> StreamName               -- Log stream
            -> Maybe Text               -- Sequence number
            -> NonEmpty (UTCTime, Text) -- Log Texts (must be chronolgically ordered)
            -> m PutLogEventsResponse
writeLogNel (GroupName groupName) (StreamName streamName) sequenceToken logs
 = liftAWS
 $ send
 $ putLogEvents groupName streamName (mkLog <$> logs)
 & pleSequenceToken .~ sequenceToken
  where
    mkLog (t,l) = inputLogEvent (mkTime t) l
    mkTime = round . (*1000) . utcTimeToPOSIXSeconds
