{-# LANGUAGE TypeApplications #-}

module Main where

import CSDC.API (API, serveAPI)
import CSDC.Config (Config (..), readConfig, showConfig)
import CSDC.ORCID (getToken, authenticationMiddleware)
import CSDC.Network.Mock (Store, makeEmptyStore, runMock)

import Control.Concurrent.MVar (MVar)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serve, hoistServer)
import System.Environment (getArgs)

import qualified Network.Wai.Middleware.Cors as Cors

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      error "First argument must be the configuration file path."
    path:_ ->
      readConfig path >>= \case
        Nothing ->
          error "Could not parse the configuration file."
        Just config -> do
          putStrLn "Running the server with the following configuration:\n"
          showConfig config
          putStrLn ""
          mainWith config

mainWith :: Config -> IO ()
mainWith config = do
  store <- makeEmptyStore
  middleware <- makeMiddleware config
  withStdoutLogger $ \logger -> do
    let port = config_port config
        path = config_path config
        settings = setPort port $ setLogger logger defaultSettings
    runSettings settings $ middleware $ application path store

makeMiddleware :: Config -> IO Middleware
makeMiddleware config = do
  authentication <- authenticationMiddleware (config_orcid config)
  let corsOptions = Cors.simpleCorsResourcePolicy
       { Cors.corsRequestHeaders = Cors.simpleHeaders }
      cors = Cors.cors (\_ -> Just corsOptions)
  pure $ authentication . cors

application :: FilePath -> MVar Store -> Application
application path store = \request response ->
  let
    proxy = Proxy @API
    token = getToken request
    server = hoistServer proxy (runMock store) (serveAPI path)
  in
    serve proxy server request response
