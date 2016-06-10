{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Data (
    Following (..)
  , LogGroup (..)
  , LogStream (..)
  , Sequence (..)
  , ExclusiveSequence (..)
  , Log (..)
  , Query (..)
  , sizeOf
  , utcToUnix
  , unixToUtc
  , newExclusiveSequence
  , safety
  , fudge
  ) where

import           Control.Lens (over, set)
import           Control.Concurrent.MVar (MVar, newMVar)

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import           Mismi.Amazonka (Env, serviceRetry, retryAttempts, exponentBase, configure)
import           Mismi.CloudwatchLogs.Amazonka (cloudWatchLogs)
import           Numeric.Natural (Natural)

import           P

import           System.IO (IO)

import           Twine.Data (Duration)


data Following =
    Follow !Duration
  | NoFollow
    deriving (Eq, Show)

newtype LogGroup =
  LogGroup {
      logGroup :: Text
    } deriving (Eq, Show)

newtype LogStream =
  LogStream {
      logStream :: Text
    } deriving (Eq, Show)

newtype Sequence =
  Sequence {
      getSequence :: Text
    } deriving (Eq, Show)

newtype ExclusiveSequence =
  ExclusiveSequence {
      exclusiveSequence :: MVar (Maybe Sequence)
    }

data Log =
  Log {
      logChunk :: !Text
    , logTime :: !UTCTime
    } deriving (Eq, Show)

data Query =
    Everything
  | From UTCTime
  | To UTCTime
  | Between UTCTime UTCTime
  | At Sequence
    deriving (Eq, Show)

-- |
-- 26 bytes of overhead per event plus size of data
--
-- http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html
--
sizeOf :: Log -> Int
sizeOf l =
  (B.length . T.encodeUtf8 . logChunk $ l) + 26


-- |
-- A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
utcToUnix :: UTCTime -> Natural
utcToUnix =
  round . (*1000) . utcTimeToPOSIXSeconds


unixToUtc :: Natural -> UTCTime
unixToUtc =
  posixSecondsToUTCTime . fromIntegral . (`div` 1000)

newExclusiveSequence :: Maybe Sequence -> IO ExclusiveSequence
newExclusiveSequence s =
  ExclusiveSequence <$> newMVar s

safety :: Env -> Env
safety =
 configure (over serviceRetry (set retryAttempts 10 . set exponentBase 0.6) cloudWatchLogs)


-- |
-- Ensure logs stay in their current order but all the times are
-- in _reverse_ chronological order. This is useful / required to
-- handle clock jitter where ordered log events get associated with
-- out-of-order timestamps, you want the logs to stay in order but
-- the timestamp fudged to maintain the chronology.
--
fudge :: [Log] -> [Log]
fudge =
  snd . foldr (\el (last, acc) ->
    case last of
      Just x ->
        if diffUTCTime (logTime x) (logTime el) < 0
           then (Just el, el:acc)
           else let el' = Log (logChunk el) (logTime x) in (Just el', el':acc)
      Nothing ->
        (Just el, el:acc)
    ) (Nothing, [])
