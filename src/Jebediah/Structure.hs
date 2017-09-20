{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Jebediah.Structure (
    listLogGroups
  , sourceLogGroups
  , newLogGroup
  , createLogGroup
  , listLogStreams
  , sourceLogStreams
  , newLogStream
  , createLogStream
  ) where

import           Control.Lens ((.~), (^.))

import           Data.Conduit (Source, ($$), (=$=))
import qualified Data.Conduit.List as C

import           Jebediah.Data

import           Mismi (AWS)
import qualified Mismi.Amazonka as M
import qualified Mismi.CloudwatchLogs.Amazonka as M

import           P


listLogGroups :: Maybe LogGroup -> AWS [M.LogGroup]
listLogGroups prefix =
  sourceLogGroups prefix $$ C.consume

sourceLogGroups :: Maybe LogGroup -> Source AWS M.LogGroup
sourceLogGroups prefix =
  let
    source =
      M.paginate $
        M.describeLogGroups & M.dlgLogGroupNamePrefix .~ fmap logGroup prefix
  in
    source =$= C.concatMap (^. M.dlgrsLogGroups)

createLogGroup :: LogGroup -> AWS ()
createLogGroup =
  void . M.send . M.createLogGroup . logGroup

newLogGroup :: LogGroup -> AWS ()
newLogGroup g = do
  exists <- fmap (not . null) $ listLogGroups (Just g)
  unless exists $
    createLogGroup g

listLogStreams :: LogGroup -> StreamOutput -> AWS [M.LogStream]
listLogStreams group output =
  sourceLogStreams group output $$ C.consume

sourceLogStreams :: LogGroup -> StreamOutput -> Source AWS M.LogStream
sourceLogStreams group output =
  let
    (order, prefix, descending) = case output of
                        StreamsPrefix p ->
                          (M.LogStreamName, p, False)
                        StreamsLatest ->
                          (M.LastEventTime, Nothing, True)

    source =
      M.paginate $
        M.describeLogStreams (logGroup group)
          & M.dlssOrderBy .~ (Just order)
          & M.dlssDescending .~ (Just descending)
          & M.dlssLogStreamNamePrefix .~ fmap logStream prefix
  in
    source =$= C.concatMap (^. M.dlsrsLogStreams)

createLogStream :: LogGroup -> LogStream -> AWS ()
createLogStream group stream =
  void . M.send . M.createLogStream (logGroup group) . logStream $ stream

newLogStream :: LogGroup -> LogStream -> AWS ()
newLogStream g s = do
  exists <- fmap (not . null) $ listLogStreams g (StreamsPrefix $ Just s)
  unless exists $
    createLogStream g s
