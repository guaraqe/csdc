module CSDC.SQL
  ( -- * Config
    Config (..)
    -- * Error
  , Error (..)
    -- * Action
  , Action (..)
  , run
  , query
  ) where

import Control.Exception (Exception, finally, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Data.Text (Text)
import Hasql.Connection (Connection, ConnectionError)
import Hasql.Session (QueryError)
import Hasql.Statement (Statement)

import qualified Data.Text.Encoding as Text.Encoding
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session

--------------------------------------------------------------------------------
-- Config

-- | The configuration of the PostgreSQL server.
data Config = Config
  { config_host :: Text
  , config_port :: Int
  , config_user :: Text
  , config_password :: Text
  , config_database :: Text
  } deriving (Show, Eq)

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
run :: MonadIO m => Config -> Action a -> m (Either Error a)
run config (Action m) = liftIO $ do
  let settings =
        Connection.settings
          (Text.Encoding.encodeUtf8 $ config_host config)
          (fromIntegral $ config_port config)
          (Text.Encoding.encodeUtf8 $ config_user config)
          (Text.Encoding.encodeUtf8 $ config_password config)
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
