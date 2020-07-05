module CSDC.SQL
  ( -- * Config and Secret
    Config (..)
  , Secret (..)
    -- * Context
  , Context
  , activate
    -- * Error
  , Error (..)
    -- * Action
  , Action (..)
  , run
  , query
  ) where

import CSDC.Prelude

import Control.Exception (Exception, finally, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Hasql.Connection (Connection, ConnectionError)
import Hasql.Session (QueryError)
import Hasql.Statement (Statement)

import qualified Data.Text.Encoding as Text.Encoding
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session

--------------------------------------------------------------------------------
-- Config and Secret

-- | The configuration of the PostgreSQL server.
data Config = Config
  { config_host :: Text
  , config_port :: Int
  , config_user :: Text
  , config_database :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

-- | The secrets of the PostgreSQL server.
data Secret = Secret
  { secret_password :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Secret

--------------------------------------------------------------------------------
-- Context

-- | The context of the PostgreSQL server.
data Context = Context
  { context_config :: Config
  , context_secret :: Secret
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Context

-- | Activate the configuration.
-- TODO: Check the validity of the configuration.
activate :: Config -> Secret -> IO Context
activate config secret = pure $ Context config secret

--------------------------------------------------------------------------------
-- Error

-- | The errors that can be emitted from SQL actions.
data Error
  = ErrorConnection ConnectionError
  | ErrorQuery QueryError
    deriving (Show, Eq)

instance Exception Error

--------------------------------------------------------------------------------
-- Action

-- | An action performing SQL operations.
newtype Action a = Action (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad)

-- | Run a SQL action and return possible errors.
run :: MonadIO m => Context -> Action a -> m (Either Error a)
run (Context config secret) (Action m) = liftIO $ do
  let settings =
        Connection.settings
          (Text.Encoding.encodeUtf8 $ config_host config)
          (fromIntegral $ config_port config)
          (Text.Encoding.encodeUtf8 $ config_user config)
          (Text.Encoding.encodeUtf8 $ secret_password secret)
          (Text.Encoding.encodeUtf8 $ config_database config)

  Connection.acquire settings >>= \case
    Left err ->
      pure $ Left $ ErrorConnection err

    Right conn ->
      try (runReaderT m conn) `finally` (Connection.release conn)

-- | Lift a SQL statement into an action.
query :: Statement a b -> a -> Action b
query stm a = Action $ do
  conn <- ask
  let session = Session.statement a stm
  liftIO (Session.run session conn) >>= \case
    Left err ->
      liftIO $ throwIO $ ErrorQuery err
    Right res ->
      pure res
