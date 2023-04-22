{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import CSDC.API (serveAPI)
import CSDC.API.Auth qualified as Auth
import CSDC.Action qualified as Action
import CSDC.Config (Context (..), activate, readConfig, showConfig)
import CSDC.Daemon qualified as Daemon
import CSDC.Daemon.Mail qualified as Daemon.Mail
import CSDC.SQL qualified as SQL
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
import Servant (Application)
import Servant.Server.Generic (genericServeTWithContext)
import System.Environment (getArgs)
import System.IO

args :: IO FilePath
args =
  getArgs >>= \case
    configPath : [] ->
      pure configPath
    _ ->
      error "Usage: csdc-server CONFIG_PATH [SECRET_PATH]"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  configPath <- args
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
