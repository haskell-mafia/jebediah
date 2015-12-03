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
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Mismi
import           Mismi.CloudwatchLogs.Amazonka hiding (createLogGroup, createLogStream)
import qualified Mismi.CloudwatchLogs.Amazonka as MA

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Text (Text)
import           Data.List.NonEmpty
--import           Data.Text.IO (putStrLn)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Network.AWS hiding (runAWS, await)

import           Jebediah.Data

listLogGroups :: MonadAWS m
              => Maybe Text
              -> m [LogGroup]
listLogGroups prefixName
  = liftAWS
  $ flip ($$) DC.consume
  $ listLogGroups' prefixName

listLogGroups' :: Maybe Text
               -> Source AWS LogGroup
listLogGroups' prefixName
  = flip (=$=) (DC.concatMap (^. dlgrsLogGroups))
  $ paginate
  $ describeLogGroups
  & dlgLogGroupNamePrefix  .~ prefixName

createLogGroup :: MonadAWS m
               => Text
               -> m ()
createLogGroup
  = liftAWS
  . fmap (const ()) . send
  . MA.createLogGroup

listLogStreams :: MonadAWS m
               => Text
               -> Maybe Text
               -> m [LogStream]
listLogStreams groupName prefixName
  = liftAWS
  $ flip ($$) DC.consume
  $ listLogStreams' groupName prefixName

listLogStreams' :: Text
                -> Maybe Text
                -> Source AWS LogStream
listLogStreams' groupName prefixName
  = flip (=$=) (DC.concatMap (^. dlsrsLogStreams))
  $ paginate
  $ describeLogStreams groupName
  & dlssLogStreamNamePrefix .~ prefixName

createLogStream :: MonadAWS m
                => Text
                -> Text
                -> m ()
createLogStream g
  = liftAWS
  . fmap (const ()) . send
  . MA.createLogStream g

-- getLogEvents does *not* implement pagination, so I'm doing it myself here.
retrieveLogStream' :: Text
                   -> Text
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
        ([], Follow)   -> do
          -- Pause for 10 seconds before making the next request.
          liftIO (threadDelay 10000000)
          retrieveLogStream' groupName streamName start end (Just nxt') following
        (_, _)   -> do
          retrieveLogStream' groupName streamName start end (Just nxt') following

retrieveLogStream'' :: Text
                    -> Text
                    -> Maybe UTCTime
                    -> Maybe UTCTime
                    -> Maybe Text
                    -> AWS GetLogEventsResponse
retrieveLogStream'' groupName streamName start end Nothing
 = send
 $ getLogEvents groupName streamName
 & gleStartTime     .~ start'
 & gleEndTime       .~ end'
 & gleStartFromHead .~ (Just True)
  where
    --  A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
    start' = round . (*1000) . utcTimeToPOSIXSeconds <$> start
    end'   = round . (*1000) . utcTimeToPOSIXSeconds <$> end

retrieveLogStream'' groupName streamName _ _ x@(Just _)
 = send
 $ getLogEvents groupName streamName
 & gleNextToken .~ x

-- Conduit sink which takes lines pairs, batches them into sizes of n, and sends them up.
-- Takes care to ensure sequence tokens are used for separate jobs, but will generally be
-- called initially with Nothing for the token parameter.
logSink :: Int
        -> Text
        -> Text
        -> Maybe Text
        -> Sink (UTCTime, Text) AWS ()
logSink n groupName streamName initialSequenceNumber = buffer =$ logSinkNel groupName streamName initialSequenceNumber
  where
    buffer = do
      a <- await
      case a of
        Nothing -> return ()
        Just a' -> do
          as <- replicateM (n - 1) await
          yield (a' :| catMaybes as)
          buffer

-- Conduit sink which takes in a single NEL and pushes it up.
-- This function takes care to use the next sequence token each
-- time it is run.
logSinkNel :: Text
           -> Text
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
            => Text                     -- Log group
            -> Text                     -- Log stream
            -> Maybe Text               -- Sequence number
            -> NonEmpty (UTCTime, Text) -- Log Texts (must be chronolgically ordered)
            -> m PutLogEventsResponse
writeLogNel groupName streamName sequenceToken logs
 = liftAWS
 $ send
 $ putLogEvents groupName streamName (mkLog <$> logs)
 & pleSequenceToken .~ sequenceToken
  where
    mkLog (t,l) = inputLogEvent (mkTime t) l
    mkTime = round . (*1000) . utcTimeToPOSIXSeconds
