{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Jebediah.Structure where

import           Control.Monad.Catch (bracket_)

import           Disorder.Core.IO

import           Jebediah.Data
import           Jebediah.Structure

import           Mismi (Region (Sydney), getRegionFromEnv, discoverAWSEnvWithRegion, rawRunAWS)
import           Mismi.Amazonka (Env, send)
import qualified Mismi.CloudwatchLogs.Amazonka as M

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Jebediah.Arbitrary ()


import           X.Control.Monad.Trans.Either (eitherT)

prop_new_delete :: LogGroup -> LogStream -> Property
prop_new_delete group stream  =
  testIO $ do
    env <- testenv
    bracket_ (rawRunAWS env $ newLogGroup group >> newLogStream group stream) (deleteLogStream env group stream) $
      pure True

prop_new_new_delete :: LogGroup -> LogStream -> Property
prop_new_new_delete group stream  =
  testIO $ do
    env <- testenv
    bracket_ (rawRunAWS env $ newLogGroup group >> newLogStream group stream) (deleteLogStream env group stream) $ do
      rawRunAWS env $ newLogGroup group >> newLogStream group stream
      pure True

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
