{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CSDC.Action
  ( -- * Action
    Action,
    ActionAuth,
    run,
    run_,
    withPerson,
    throw,

    -- * Error
    Error (..),

    -- * Context
    Context (..),

    -- * Server
    Server,
    ServerAuth,

    -- * SQL
    runSQL,
    runQuery,

    -- * Mail
    runMail,

    -- * IPFS
    runIPFS,
  )
where

import CSDC.IPFS qualified as IPFS
import CSDC.Mail qualified as Mail
import CSDC.Prelude
import CSDC.SQL qualified as SQL
import Control.Exception (Exception, try)
import Control.Monad.Except (MonadError (..), throwError)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Hasql.Statement (Statement)
import Servant (ServerError, err401, err500)
import Servant.Server.Generic (AsServerT)
import UnliftIO (MonadUnliftIO, throwIO)

--------------------------------------------------------------------------------
-- Context

data Context user = Context
  { sql :: SQL.Context,
    mail :: Maybe Mail.Context,
    ipfs :: IPFS.Context,
    user :: user
  }
  deriving (Generic)

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
  let serverErr :: ServerError
      serverErr = case err of
        Unauthorized -> err401
        _ -> err500
   in throwIO serverErr

--------------------------------------------------------------------------------
-- Server

type Server api = api (AsServerT (Action ()))

type ServerAuth api = api (AsServerT (Action (Id Person)))

--------------------------------------------------------------------------------
-- Action

newtype Action user a = Action (ReaderT (Context user) IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Context user),
      MonadIO,
      MonadUnliftIO
    )

-- Actions with authentication needed
type ActionAuth = Action (Id Person)

run ::
  (MonadIO m, MonadError ServerError m) =>
  Context user ->
  Action user a ->
  m a
run ctx (Action act) =
  liftIO (try (runReaderT act ctx)) >>= \case
    Left e -> throwError e
    Right a -> pure a

run_ :: Context user -> Action user a -> IO a
run_ ctx (Action act) = runReaderT act ctx

withPerson :: Id Person -> ActionAuth a -> Action () a
withPerson pid (Action (ReaderT act)) =
  Action $ ReaderT $ \ctx -> act $ ctx {user = pid}

runSQL :: SQL.Action a -> Action user a
runSQL act = do
  ctx <- asks (.sql)
  SQL.run ctx act >>= \case
    Left e ->
      liftIO $ throwIO $ ErrorSQL e
    Right a ->
      pure a

runQuery :: Statement a b -> a -> Action user b
runQuery statement = runSQL . SQL.query statement

runMail :: Mail.Action a -> Action user a
runMail act = do
  ctx <- asks (.mail)
  Mail.run ctx act

runIPFS :: IPFS.Action a -> Action user a
runIPFS act = do
  ctx <- asks (.ipfs)
  IPFS.run ctx act
