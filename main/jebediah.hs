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

import           Jebediah.Data
import           Jebediah.Control

import qualified Data.Attoparsec.Text as A

import           Data.Conduit
import qualified Data.Conduit.List as DC
import qualified Data.Time as DT

import qualified Data.Text as T
import qualified Data.Text.IO as T

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
              (Cat <$> groupName' <*> streamName' <*> fromTime' <*> follow')

run :: Command -> IO ()
run c = do
  e <- orDie renderRegionError discoverAWSEnv
  orDie renderError . runAWS e $ case c of
    ListGroups ->
       listLogGroups' Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. lgLogGroupName))
    ListStreams g ->
       listLogStreams' g Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. lsLogStreamName))
    Cat g s tt f -> do
       tz <- liftIO DT.getCurrentTimeZone
       let tt' = (DT.localTimeToUTC tz) <$> tt
       retrieveLogStream' g s tt' Nothing Nothing f $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. oleMessage))

data Command =
  ListGroups
  | ListStreams T.Text
  | Cat T.Text T.Text (Maybe DT.LocalTime) Following
  deriving (Eq, Show)


groupName' :: Parser T.Text
groupName' = argument textRead (metavar "GROUP-NAME")

streamName' :: Parser T.Text
streamName' = argument textRead (metavar "STREAM-NAME")

fromTime' :: Parser (Maybe DT.LocalTime)
fromTime' = optional $ option (pOption p) (short 't' <> long "time" <> help "Local time floor for query from")
  where
    p = DT.LocalTime
     <$> (DT.fromGregorian <$> A.decimal <* A.char '-' <*> A.decimal <* A.char '-' <*> A.decimal)
     <* A.char 'T'
     <*> (DT.TimeOfDay <$> A.decimal <* A.char ':' <*> A.decimal <* A.char ':' <*> (fromRational <$> A.rational))

follow' :: Parser Following
follow' = flag NoFollow Follow (short 'f' <> long "follow" <> help "Whether to follow the stream (wait time 10)")
