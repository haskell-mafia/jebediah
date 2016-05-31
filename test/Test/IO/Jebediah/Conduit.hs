{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Jebediah.Conduit where

import           Control.Monad.Catch (bracket_)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit.List as C
import           Data.Time (getCurrentTime)

import           Disorder.Core.IO

import           Jebediah.Data
import           Jebediah.Structure
import qualified Jebediah.Conduit as J

import           Mismi (Region (Sydney), getRegionFromEnv, discoverAWSEnvWithRegion, rawRunAWS)
import           Mismi.Amazonka (Env, send)
import qualified Mismi.CloudwatchLogs.Amazonka as M

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Jebediah.Arbitrary


import           X.Control.Monad.Trans.Either (eitherT)

import           Twine.Snooze (snooze, seconds)

prop_read_write :: LogGroup -> LogStream -> Line -> [Line] -> Property
prop_read_write group stream x xs =
  testIO $ do
    time <- (unixToUtc . utcToUnix) <$>  getCurrentTime
    env <- testenv
    bracket_ (rawRunAWS env $ newLogGroup group >> newLogStream group stream) (deleteLogStream env group stream) $ do
      lock <- newExclusiveSequence Nothing
      let
        sink = J.sink env group stream lock
        source = J.source env group stream Everything NoFollow
        input = fmap (flip Log time . getContent) $ x:xs
      void . runResourceT $ C.sourceList input =$= J.clean $$ sink
      snooze . seconds $ 2
      result <- source =$= J.unclean $$ C.consume
      pure $ result === input

deleteLogStream :: Env -> LogGroup -> LogStream -> IO ()
deleteLogStream env group stream =
  rawRunAWS env . void . send $ M.deleteLogStream (logGroup group) (logStream stream)

testenv :: IO Env
testenv = do
  r <- eitherT (const $ pure Sydney) pure getRegionFromEnv
  safety <$> discoverAWSEnvWithRegion r

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
