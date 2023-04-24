{-# LANGUAGE OverloadedStrings #-}

module CSDC.Types.Id
  ( Id (..),
    getId,
    newId,
    WithId (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Web.Internal.HttpApiData (FromHttpApiData (..))

-- | A unique identifier for some type.
newtype Id a = Id {getId :: UUID}
  deriving newtype (Show, Read, Eq, Ord, ToJSON, FromJSON)

instance FromHttpApiData (Id a) where
  parseUrlPiece txt =
    case UUID.fromText txt of
      Nothing -> Left $ "Invalid UUID: " <> txt
      Just uuid -> pure $ Id uuid

getId :: Id a -> UUID
getId (Id a) = a

newId :: MonadIO m => m (Id a)
newId = Id <$> liftIO UUID.nextRandom

data WithId a = WithId
  { id :: Id a,
    value :: a
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
