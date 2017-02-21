{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_jebediah
import           DependencyInfo_ambiata_jebediah

import           Options.Applicative

import           P

import           Control.Lens hiding (argument)
import           Control.Concurrent.MVar (newMVar)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (runResourceT)

import           System.IO
import           System.IO.Error
import           System.Directory
import           System.Exit
import           X.Options.Applicative
import           X.Control.Monad.Trans.Either.Exit

import           Jebediah.Data
import           Jebediah.Structure
import           Jebediah.Conduit

import qualified Data.Attoparsec.Text as A

import           Data.Conduit
import qualified Data.Conduit.List as DC
import qualified Data.Time as DT

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi (discoverAWSEnv, renderRegionError, runAWS, renderError)
import           Mismi.Amazonka (serviceRetry, retryAttempts, exponentBase, configure)
import           Mismi.CloudwatchLogs.Amazonka (cloudWatchLogs)
import qualified Mismi.CloudwatchLogs.Amazonka as M

import           Twine.Data (seconds)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      DependencyCommand ->
        for dependencyInfo putStrLn >> exitSuccess
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
  <> command' "upload-file-to-existing"
              "Upload a file to an existing stream"
              (UploadFile <$> groupName' <*> streamName' <*> argument str (metavar "FILEPATH") <*> sequenceNumber')

run :: Command -> IO ()
run c = do
  ee <- orDie renderRegionError discoverAWSEnv
  let e = configure (over serviceRetry (set retryAttempts 10 . set exponentBase 1) cloudWatchLogs) ee
  orDie renderError . runAWS e $ case c of
    ListGroups ->
       sourceLogGroups Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. M.lgLogGroupName))
    ListStreams g ->
       sourceLogStreams g Nothing $$ DC.mapM_ (\x -> liftIO $ T.putStrLn `traverse_` (x ^. M.lsLogStreamName))
    Cat g s tt f -> liftIO $ do
       tz <- DT.getCurrentTimeZone
       let tt' = case tt of Nothing -> Everything; Just ttt -> From (DT.localTimeToUTC tz ttt)
       source e g s tt' f $$ DC.mapM_ (\(Log text _) -> liftIO $ T.putStrLn text)
    CreateGroup g ->
      newLogGroup g
    CreateStream g s ->
      newLogStream g s
    CreateStreamAndUpload g s fp ->
      liftIO (doesFileExist fp) >>= \case
        True -> do
          createLogStream g s
          lock <- liftIO . fmap ExclusiveSequence . newMVar $ Nothing
          void . liftIO . runResourceT $ getFileConduit fp $$ sink e g s lock
        False -> liftIO $ do
          putStrLn "File does not exist"
          exitWith (ExitFailure 1)
    UploadFile g s fp sn -> do
      lock <- liftIO $ ExclusiveSequence <$> newMVar (Sequence <$> sn)
      void . liftIO . runResourceT $ getFileConduit fp $$ sink e g s lock

data Command =
  ListGroups
  | ListStreams LogGroup
  | Cat LogGroup LogStream (Maybe DT.LocalTime) Following
  | CreateGroup LogGroup
  | CreateStream LogGroup LogStream
  | CreateStreamAndUpload LogGroup LogStream FilePath
  | UploadFile LogGroup LogStream FilePath (Maybe T.Text)
  deriving (Eq, Show)

groupName' :: Parser LogGroup
groupName' = LogGroup <$> argument textRead (metavar "GROUP-NAME")

streamName' :: Parser LogStream
streamName' = LogStream <$> argument textRead (metavar "STREAM-NAME")

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
follow' = (Follow . seconds) <$> option auto (short 'f' <> long "follow" <> help "Follow the stream with checks every 'X' seconds" <> metavar "X") <|> pure NoFollow

getFileConduit :: MonadIO m => FilePath -> Source m Log
getFileConduit path = do
  h <- liftIO (openFile path ReadMode)
  getFileLines' h
    where
      getFileLines' h = do
        a <- liftIO $ tryIOError (T.hGetLine h)
        t <- liftIO DT.getCurrentTime
        case a of
          Right x -> yield (Log x t) >> getFileLines' h
          Left _  -> return ()
