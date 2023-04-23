{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import CSDC.API (serveAPI)
import CSDC.API.Auth qualified as Auth
import CSDC.Action qualified as Action
import CSDC.Config (Context (..), activate, readConfig, showConfig)
import CSDC.Daemon qualified as Daemon
import CSDC.Daemon.Mail qualified as Daemon.Mail
import CSDC.SQL qualified as SQL
import CSDC.SQL.Subparts qualified as SQL.Subparts
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
import Options.Generic
import Servant (Application)
import Servant.Server.Generic (genericServeTWithContext)
import System.IO

data Commands
  = Serve { config :: FilePath }
  | Test { config :: FilePath, unitId :: String }
  deriving (Generic, Show)

instance ParseRecord Commands

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getRecord "csdc-server" >>= \case
    Serve configPath ->
      readConfig configPath >>= \case
        Left e ->
          error $ "Could not parse the configuration file: " <> e
        Right config -> do
          putStrLn "Starting the server with the following configuration:\n"
          showConfig config
          putStrLn ""
          context <- activate config
          putStrLn "Applying migrations..."
          migrate context
          mainWith context
    Test configPath unitId ->
      readConfig configPath >>= \case
        Left e ->
          error $ "Could not parse the configuration file: " <> e
        Right config -> do
          Context {..} <- activate config
          Action.run_ dao $ do
            ids <- Action.runQuery SQL.Subparts.selectByExtendedParent (read unitId)
            liftIO $ print ids

mainWith :: Context -> IO ()
mainWith Context {..} = do
  putStrLn "Starting mail daemon..."
  _ <- Action.run_ dao $ do
    Daemon.launch Daemon.Mail.daemon
  putStrLn "Server ready."
  withStdoutLogger $ \logger -> do
    let settings = setPort port $ setLogger logger defaultSettings
    authSettings <- Auth.makeSettings jwkPath
    runSettings settings $
      middleware $
        application path authSettings dao

middleware :: Middleware
middleware =
  let corsOptions =
        Cors.simpleCorsResourcePolicy
          { Cors.corsRequestHeaders = Cors.simpleHeaders
          }
      cors = Cors.cors (\_ -> Just corsOptions)
      compress = gzip def {gzipFiles = GzipCompress}
   in compress . cors

application :: FilePath -> Auth.Settings -> Action.Context () -> Application
application path settings context =
  let sqlContext = context.sql
      cfg = Auth.makeContext settings
   in genericServeTWithContext (Action.run context) (serveAPI path sqlContext settings) cfg

migrate :: Context -> IO ()
migrate context = do
  let path = context.migration
  Action.run_ context.dao $ Action.runSQL $ SQL.migrate path
