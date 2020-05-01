{-# LANGUAGE LambdaCase #-}

module CSDC.User
  ( -- * Type
    User (..)
  , UserId
    -- * Class
  , HasUser (..)
    -- * Transformer
  , UserT
  , runUserT
  ) where

import CSDC.Auth (UserToken)
import CSDC.Auth.User (User (..))
import CSDC.Network.Class (HasNetwork (..))
import CSDC.Network.Types (Person (..))
import CSDC.Data.Id (Id)

import qualified CSDC.Auth.ORCID as ORCID

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))

--------------------------------------------------------------------------------
-- Class

type UserId = User (Id Person)

-- | A class for monads that have an user in scope.
class Monad m => HasUser m where
  -- | Get the current user in scope.
  getUser :: m UserId

--------------------------------------------------------------------------------
-- Transformer

-- | A transformer that adds the 'HasUser' capability to another monad.
newtype UserT m a = UserT (ReaderT UserToken m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , HasNetwork
    )

instance HasNetwork m => HasUser (UserT m) where
  getUser = UserT $
    ask >>= \case
      Admin ->
        pure Admin

      User token ->
        selectPersonORCID (ORCID.token_orcid token) >>= \case
          Nothing ->
            let
              person = Person
                { person_name = ORCID.token_name token
                , person_orcid = ORCID.token_orcid token
                }
            in
              User <$> insertPerson person
          Just uid ->
            pure $ User uid

-- | Execute an 'UserT' in its base monad.
runUserT :: UserToken -> UserT m a -> m a
runUserT user (UserT act) = runReaderT act user
