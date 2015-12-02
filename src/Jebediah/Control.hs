{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Control (
    listLogGroups
  , listLogGroups'
  , listLogStreams
  , listLogStreams'
  , retrieveLogStream'
  ) where

import           P
import           Control.Concurrent (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Mismi
import           Mismi.CloudwatchLogs.Amazonka

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Text (Text)
--import           Data.Text.IO (putStrLn)
import           Data.Time.Clock.POSIX

import           Network.AWS hiding (runAWS)
import           Network.AWS.Data.Time

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
    start' = (*1000) . round . utcTimeToPOSIXSeconds <$> start
    end'   = (*1000) . round . utcTimeToPOSIXSeconds <$> end

retrieveLogStream'' groupName streamName _ _ x@(Just _)
 = send
 $ getLogEvents groupName streamName
 & gleNextToken .~ x
