{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_jebediah

import           Options.Applicative

import           P

import           Control.Lens hiding (argument)
import           Control.Monad.IO.Class

import           System.IO
import           System.IO.Error
import           System.Directory
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
import           Mismi.CloudwatchLogs.Amazonka hiding (createLogGroup, createLogStream)


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
              "List log streams in a log group"
              (ListStreams <$> groupName')
  <> command' "cat-stream"
              "Cat a stream"
              (Cat <$> groupName' <*> streamName' <*> fromTime' <*> follow')
  <> command' "create-group"
              "Create a log group"
              (CreateGroup <$> groupName')
  <> command' "create-stream"
              "Create a log stream in a group"
              (CreateStream <$> groupName' <*> streamName')
  <> command' "upload-file"
              "Upload a file to a new fresh stream"
              (CreateStreamAndUpload <$> groupName' <*> streamName' <*> argument str (metavar "FILEPATH"))
  <> command' "upload-file-to-exising"
              "Upload a file to an existing stream"
              (UploadFile <$> groupName' <*> streamName' <*> argument str (metavar "FILEPATH") <*> sequenceNumber')

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
    CreateGroup g ->
      createLogGroup g
    CreateStream g s ->
      createLogStream g s
    CreateStreamAndUpload g s fp ->
      liftIO (doesFileExist fp) >>= \case
        True -> do
          createLogStream g s
          getFileConduit fp $$ logSink 100 g s Nothing
        False -> liftIO $ do
          putStrLn "File does not exist"
          exitWith (ExitFailure 1)
    UploadFile g s fp sn ->
      getFileConduit fp $$ logSink 100 g s sn

data Command =
  ListGroups
  | ListStreams GroupName
  | Cat GroupName StreamName (Maybe DT.LocalTime) Following
  | CreateGroup GroupName
  | CreateStream GroupName StreamName
  | CreateStreamAndUpload GroupName StreamName FilePath
  | UploadFile GroupName StreamName FilePath (Maybe T.Text)
  deriving (Eq, Show)

groupName' :: Parser GroupName
groupName' = GroupName <$> argument textRead (metavar "GROUP-NAME")

streamName' :: Parser StreamName
streamName' = StreamName <$> argument textRead (metavar "STREAM-NAME")

sequenceNumber' :: Parser (Maybe T.Text)
sequenceNumber' = optional $ option textRead (long "sequence-number")

fromTime' :: Parser (Maybe DT.LocalTime)
fromTime' = optional $ option (pOption p) (short 't' <> long "time" <> help "Local time floor for query from")
  where
    p = DT.LocalTime
     <$> (DT.fromGregorian <$> A.decimal <* A.char '-' <*> A.decimal <* A.char '-' <*> A.decimal)
     <* A.char 'T'
     <*> (DT.TimeOfDay <$> A.decimal <* A.char ':' <*> A.decimal <* A.char ':' <*> (fromRational <$> A.rational))

follow' :: Parser Following
follow' = Follow <$> option auto (short 'f' <> long "follow" <> help "Follow the stream with checks every 'X' seconds" <> metavar "X") <|> pure NoFollow

getFileConduit :: MonadIO m => FilePath -> Source m (DT.UTCTime, T.Text)
getFileConduit path = do
  h <- liftIO (openFile path ReadMode)
  getFileLines' h
    where
      getFileLines' h = do
        a <- liftIO $ tryIOError (T.hGetLine h)
        t <- liftIO DT.getCurrentTime
        case a of
          Right x -> yield (t,x) >> getFileLines' h
          Left _  -> return ()
