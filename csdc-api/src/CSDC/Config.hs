{-# LANGUAGE StrictData #-}

module CSDC.Config
  ( -- * Config
    Config (..)
  , readConfig
  , showConfig
    -- * Secret
  , Secret (..)
  , readSecret
    -- * Context
  , Context (..)
  , activate
  ) where

import CSDC.Prelude

import qualified CSDC.Auth as Auth
import qualified CSDC.Auth.Admin as Admin
import qualified CSDC.Auth.ORCID as ORCID

import Data.Aeson (decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.ByteString.Lazy.Char8 as ByteString

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { config_port :: Int
  , config_path :: FilePath
  , config_orcidEndpoint :: ORCID.Endpoint
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

readConfig :: MonadIO m => FilePath -> m (Maybe Config)
readConfig path = liftIO $ do
  mconfig <- decodeFileStrict path
  mport <- lookupEnv "PORT"
  case readMaybe =<< mport of
    Nothing ->
      pure mconfig
    Just port ->
      case mconfig of
        Nothing ->
          pure Nothing
        Just config ->
          pure $ Just config { config_port = port }

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let
    str = ByteString.unpack $ encodePretty config
  in
    liftIO $ putStrLn str

--------------------------------------------------------------------------------
-- Secret

data Secret = Secret
  { secret_token :: Text
  , secret_orcidId :: Text
  , secret_orcidSecret :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Secret

-- | Read secrets either from a file, or from environment variables.
readSecret :: MonadIO m => Maybe FilePath -> m (Maybe Secret)
readSecret (Just path) = liftIO $ decodeFileStrict path
readSecret Nothing = liftIO $ do
  let env var = fmap Text.pack <$> lookupEnv var
  mToken <- env "SECRET_TOKEN"
  mOrcidId <- env "SECRET_ORCID_ID"
  mOrcidSecret <- env "SECRET_ORCID_SECRET"
  pure $ Secret <$> mToken <*> mOrcidId <*> mOrcidSecret

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { context_port :: Int
  , context_path :: FilePath
  , context_auth :: Auth.Config
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Context

activate :: Config -> Secret -> IO Context
activate config secret = pure Context
  { context_port = config_port config
  , context_path = config_path config
  , context_auth = Auth.Config
      { Auth.config_orcid = ORCID.Config
          { ORCID.config_id = secret_orcidId secret
          , ORCID.config_secret = secret_orcidSecret secret
          , ORCID.config_endpoint = config_orcidEndpoint config
          }
      , Auth.config_admin = Admin.Token
          { Admin.token_value =
              Text.Encoding.encodeUtf8 (secret_token secret)
          }
      }
  }
