{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Single (
    read
  , write
  , write'
  ) where


import           Control.Lens (view, (.~))

import           Data.List.NonEmpty (NonEmpty (..))

import           Jebediah.Data

import           Mismi (AWS)
import           Mismi.Amazonka (send)
import qualified Mismi.CloudwatchLogs.Amazonka as M

import           P


read :: LogGroup -> LogStream -> Query -> AWS M.GetLogEventsResponse
read group stream query =
  let
    base = M.getLogEvents (logGroup group) (logStream stream) & M.gleStartFromHead .~ (Just True)
    configured = case query of
      Everything ->
        base
      From s ->
        base & M.gleStartTime .~ Just (utcToUnix s)
      To e ->
        base & M.gleEndTime .~ Just (utcToUnix e)
      Between s e ->
        base & M.gleStartTime .~ Just (utcToUnix s) & M.gleEndTime .~ Just (utcToUnix e)
      At s ->
        base & M.gleNextToken .~ (Just $ getSequence s)
   in
     send configured

write :: LogGroup -> LogStream -> Maybe Sequence -> [Log] -> AWS (Maybe Sequence)
write group stream next logs =
  case logs of
    [] ->
      pure Nothing
    (x : xs) ->
      write' group stream next (x :| xs)

write' :: LogGroup -> LogStream -> Maybe Sequence -> NonEmpty Log -> AWS (Maybe Sequence)
write' group stream next logs =
  fmap (fmap Sequence . view M.plersNextSequenceToken) . send $
    M.putLogEvents (logGroup group) (logStream stream) (toCloudwatch <$> logs)
      & M.pleSequenceToken .~ (fmap getSequence next)

toCloudwatch :: Log -> M.InputLogEvent
toCloudwatch l =
  M.inputLogEvent (utcToUnix . logTime $ l) (logChunk l)
