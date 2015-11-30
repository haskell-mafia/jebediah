{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_jebediah

import           Options.Applicative

import           P

import           Control.Lens hiding (argument)
import           Control.Monad.IO.Class

import           System.IO
import           System.Exit
import           X.Options.Applicative
import           X.Control.Monad.Trans.Either.Exit

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Jebediah.Control

import           Data.Conduit
import qualified Data.Conduit.List as DC

import           Mismi
import           Mismi.CloudwatchLogs.Amazonka


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

parser :: Parser (SafeCommand Command)
parser =
  safeCommand commandP'

commandP' :: Parser Command
commandP' = subparser $
     command' "list-groups"
              "List all log groups"
              (pure ListGroups)
  <> command' "list-streams"
              "List log streams in a group"
              (ListStreams <$> groupName')
  <> command' "cat-stream"
              "Cat a stream"
              (Cat <$> groupName' <*> streamName')
run :: Command -> IO ()
run c = do
  e <- orDie renderRegionError discoverAWSEnv
  orDie renderError . runAWS e $ case c of
    ListGroups ->
       listLogGroups' Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. lgLogGroupName))
    ListStreams g ->
       listLogStreams' g Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. lsLogStreamName))
    Cat g s ->
       retrieveLogStream' g s Nothing Nothing Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. oleMessage))

data Command =
  ListGroups
  | ListStreams T.Text
  | Cat T.Text T.Text
  deriving (Eq, Show)


groupName' :: Parser T.Text
groupName' = argument textRead (metavar "GROUP-NAME")

streamName' :: Parser T.Text
streamName' = argument textRead (metavar "STREAM-NAME")
