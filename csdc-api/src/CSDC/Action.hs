{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CSDC.Action
  ( -- * Action
    Action
  , ActionAuth
  , run
  , run_
  , withPerson
  , throw
    -- * Error
  , Error (..)
    -- * Context
  , Context (..)
    -- * Server
  , Server
  , ServerAuth
    -- * SQL
  , runSQL
  , runQuery
    -- * Mail
  , runMail
    -- * IPFS
  , runIPFS
  ) where

import CSDC.Prelude

import qualified CSDC.IPFS as IPFS
import qualified CSDC.Mail as Mail
import qualified CSDC.SQL as SQL

import Control.Exception (Exception, try)
import Control.Monad.Except (MonadError (..), throwError)
import Control.Monad.Reader (ReaderT (..), MonadReader (..), asks)
import Hasql.Statement (Statement)
import Servant (ServerT, ServerError, err500, err401)
import UnliftIO (MonadUnliftIO, throwIO)

--------------------------------------------------------------------------------
-- Context

data Context user = Context
  { context_sql :: SQL.Context
  , context_mail :: Maybe Mail.Context
  , context_ipfs :: IPFS.Context
  , context_user :: user
  } deriving (Generic)

--------------------------------------------------------------------------------
-- Error

data Error
  = ErrorSQL SQL.Error
  | Unauthorized
  | PersonDoesNotExist (Id Person)
  | UnitDoesNotExist (Id Unit)
    deriving (Show, Eq)

instance Exception Error

throw :: Error -> Action user a
throw err =
  let
    serverErr :: ServerError
    serverErr = case err of
      Unauthorized -> err401
      _ -> err500
  in
    throwIO serverErr

--------------------------------------------------------------------------------
-- Server

type Server api = ServerT api (Action ())

type ServerAuth api = ServerT api (Action (Id Person))

--------------------------------------------------------------------------------
-- Action

newtype Action user a = Action (ReaderT (Context user) IO a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (Context user)
    , MonadIO, MonadUnliftIO
    )

-- Actions with authentication needed
type ActionAuth = Action (Id Person)

run ::
  (MonadIO m, MonadError ServerError m) =>
  Context user -> Action user a -> m a
run ctx (Action act) =
  liftIO (try (runReaderT act ctx)) >>= \case
    Left e -> throwError e
    Right a -> pure a

run_ :: Context user -> Action user a -> IO a
run_ ctx (Action act) = runReaderT act ctx

withPerson :: Id Person -> ActionAuth a -> Action () a
withPerson pid (Action (ReaderT act)) =
  Action $ ReaderT $ \ctx -> act $ ctx { context_user = pid }

runSQL :: SQL.Action a -> Action user a
runSQL act = do
  ctx <- asks context_sql
  SQL.run ctx act >>= \case
    Left e ->
      liftIO $ throwIO $ ErrorSQL e
    Right a ->
      pure a

runQuery :: Statement a b -> a -> Action user b
runQuery statement = runSQL . SQL.query statement

runMail :: Mail.Action a -> Action user a
runMail act = do
  ctx <- asks context_mail
  Mail.run ctx act

runIPFS :: IPFS.Action a -> Action user a
runIPFS act = do
  ctx <- asks context_ipfs
  IPFS.run ctx act
