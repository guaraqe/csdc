{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module CSDC.Config
  ( -- * Config
    Config (..),
    readConfig,
    showConfig,

    -- * Context
    Context (..),
    activate,
  )
where

import CSDC.Action qualified as Action
import CSDC.IPFS qualified as IPFS
import CSDC.Mail qualified as Mail
import CSDC.Prelude
import CSDC.SQL qualified as SQL
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Text as Text
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- SQL Config

data SQLConfig = SQLConfigFile SQL.Config | SQLConfigEnv String
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

activateSQL :: SQLConfig -> IO SQL.Context
activateSQL (SQLConfigFile config) = SQL.activate config
activateSQL (SQLConfigEnv var) =
  env var >>= \case
    Nothing ->
      error $ "Could not find variable for SQL configuration named $" <> var
    Just str ->
      case SQL.parseURL (Text.unpack str) of
        Nothing ->
          error $ "Could not parse SQL configuration from $" <> var
        Just config ->
          SQL.activate config

--------------------------------------------------------------------------------
-- Mail Config

data MailConfig
  = MailConfigFile Mail.Config
  | MailConfigEnv String
  | MailConfigDisplay
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

activateMail :: MailConfig -> IO (Maybe Mail.Context)
activateMail (MailConfigFile config) = Just <$> Mail.activate config
activateMail (MailConfigEnv prefix) = do
  mHostName <- lookupEnv (prefix <> "_SMTP_SERVER")
  mPortNumber <- fmap read <$> lookupEnv (prefix <> "_SMTP_PORT")
  mUserName <- lookupEnv (prefix <> "_SMTP_LOGIN")
  mPassword <- lookupEnv (prefix <> "_SMTP_PASSWORD")
  case Mail.Config <$> mHostName <*> mPortNumber <*> mUserName <*> mPassword of
    Nothing -> error "Could not find environment variables"
    Just config -> do
      print config
      Just <$> Mail.activate config
activateMail MailConfigDisplay = pure Nothing

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { port :: Int,
    path :: FilePath,
    sql :: SQLConfig,
    mail :: MailConfig,
    ipfs :: IPFS.Config,
    migration :: FilePath,
    jwkPath :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

readConfig :: MonadIO m => FilePath -> m (Either String Config)
readConfig path = liftIO $ do
  mconfig <- eitherDecodeFileStrict path
  mport <- lookupEnv "PORT"
  case readMaybe =<< mport of
    Nothing ->
      pure mconfig
    Just port ->
      case mconfig of
        Left e ->
          pure $ Left e
        Right config ->
          pure $ Right (config {port = port} :: Config)

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let str = ByteString.unpack $ encodePretty config
   in liftIO $ putStrLn str

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { port :: Int,
    path :: FilePath,
    dao :: Action.Context (),
    migration :: FilePath,
    jwkPath :: Maybe FilePath
  }
  deriving (Generic)

activate :: Config -> IO Context
activate config = do
  sql <- activateSQL config.sql
  mail <- activateMail config.mail
  ipfs <- IPFS.activate config.ipfs
  pure
    Context
      { port = config.port,
        path = config.path,
        dao =
          Action.Context
            { Action.sql = sql,
              Action.mail = mail,
              Action.ipfs = ipfs,
              Action.user = ()
            },
        migration = config.migration,
        jwkPath = config.jwkPath
      }

--------------------------------------------------------------------------------
-- Helper

env :: String -> IO (Maybe Text)
env var = fmap Text.pack <$> lookupEnv var
