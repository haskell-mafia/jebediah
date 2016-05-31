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

listLogStreams :: LogGroup -> Maybe LogStream -> AWS [M.LogStream]
listLogStreams group prefix =
  sourceLogStreams group prefix $$ C.consume

sourceLogStreams :: LogGroup -> Maybe LogStream -> Source AWS M.LogStream
sourceLogStreams group prefix =
  let
    source =
      M.paginate $
        M.describeLogStreams (logGroup group)
          & M.dlssLogStreamNamePrefix .~ fmap logStream prefix
  in
    source =$= C.concatMap (^. M.dlsrsLogStreams)

createLogStream :: LogGroup -> LogStream -> AWS ()
createLogStream group stream =
  void . M.send . M.createLogStream (logGroup group) . logStream $ stream

newLogStream :: LogGroup -> LogStream -> AWS ()
newLogStream g s = do
  exists <- fmap (not . null) $ listLogStreams g (Just s)
  unless exists $
    createLogStream g s
