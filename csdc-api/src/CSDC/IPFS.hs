{-# LANGUAGE RecordWildCards #-}

module CSDC.IPFS
  ( -- * General
    Config (..)
  , Context (..)
  , activate
  , Action (..)
  , run
  , add
  , addJSON
    -- * Reexport
  , CID (..)
  ) where

import CSDC.Prelude

import Control.Exception (throwIO)
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Network.IPFS (MonadLocalIPFS (..))
import Network.IPFS.Add (addFile)
import Network.IPFS.CID.Types (CID (..))
import Network.IPFS.Name.Types (Name (..))
import Network.IPFS.Process.Error (Error (..))
import System.Process.Typed
import System.Exit

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { config_path :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

data Context = Context
  { context_path :: FilePath
  }

activate :: Config -> IO Context
activate Config {..} = do
  pure Context
    { context_path = config_path
    }

--------------------------------------------------------------------------------
-- Action

newtype Action a = Action (ReaderT Context IO a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader Context, MonadIO)

run :: MonadIO m => Context -> Action a -> m a
run context (Action action) = liftIO $ runReaderT action context

instance MonadLocalIPFS Action where
  runLocal opts arg = do
    Context path <- ask

    let secs = 5
        opts' = ("--timeout=" <> show secs <> "s") : opts
        process =
          setStdin (byteStringInput arg) $
          proc path opts'

    readProcess process >>= \case
      (ExitSuccess, contents, _) ->
        return $ Right contents
      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            return . Left $ Timeout secs
        | otherwise ->
          return . Left $ UnknownErr stdErr

add :: Text -> ByteString -> Action CID
add path bs =
  addFile (Lazy.fromStrict bs) (Name (Text.unpack path)) >>= \case
    Left e -> liftIO $ throwIO e
    Right (_,a) -> pure a

addJSON :: ToJSON a => Text -> a -> Action CID
addJSON path a = add path $ Lazy.toStrict $ encode a
