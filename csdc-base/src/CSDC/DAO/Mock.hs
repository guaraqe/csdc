{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module CSDC.DAO.Mock
  ( Mock
  , runMock
  , Store
  , makeEmptyStore
  ) where

import CSDC.Data.Id (Id (..), WithId (..))
import CSDC.Data.IdMap (IdMap)
import CSDC.Data.RIO (RIO, runRIO)
import CSDC.DAO.Types (Person (..), Unit (..), Member (..), Subpart (..))
import CSDC.DAO.Class
  ( HasDAO (..)
  , HasCRUD (..)
  , HasRelation (..)
  )

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.Data.IdMap as IdMap

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Lens (Lens', makeLenses, view, set, use, modifying)

--------------------------------------------------------------------------------
-- In-memory store

data Store = Store
  { _store_person :: IdMap Person Person
  , _store_unit :: IdMap Unit Unit
  , _store_member :: IdMap Member Member
  , _store_subpart :: IdMap Subpart Subpart
  , _store_root :: Id Unit
  } deriving (Show, Eq)

makeLenses ''Store

makeEmptyStore :: MonadIO m => m (MVar Store)
makeEmptyStore = liftIO $ newMVar
  Store
    { _store_person = singleton personId person
    , _store_unit = singleton unitId unit
    , _store_member = singleton memberId member
    , _store_subpart = IdMap.empty
    , _store_root = unitId
    }
  where
    singleton uid val = IdMap.insert uid val IdMap.empty

    personId = Id 0
    person = Person
      { person_name = "Mr. President"
      , person_description = "The president of the CS-DC."
      , person_orcid = ORCID.Id "dummy"
      }

    unitId = Id 0
    unit = Unit
      { unit_name = "CS-DC"
      , unit_description = "The root of the CS-DC network."
      , unit_chair = memberId
      }

    memberId = Id 0
    member = Member personId unitId

--------------------------------------------------------------------------------
-- Mock implementation

newtype Mock m a = Mock (RIO Store m a)
  deriving newtype (Functor, Applicative, Monad, MonadState Store)

runMock :: MonadIO m => MVar Store -> Mock m a -> m a
runMock var (Mock m) = runRIO var m

instance MonadIO m => HasDAO (Mock m) where
  selectPersonORCID uid =
    fmap fst <$> IdMap.find (\p -> person_orcid p == uid) <$> use store_person

  rootUnit =
    use store_root

  -- XXX: This implementation is horrible.
  createUnit personId = do
    let dummyMemberId = Id 0
        dummyUnit = Unit "" "" dummyMemberId
    unitId <- stating store_unit (IdMap.insertNew dummyUnit)
    let member = Member personId unitId
    memberId <- insertRelation @Member member
    let unit = Unit "New Unit" "Unit Description" memberId
    update @Unit unitId unit
    pure $ WithId memberId member


instance MonadIO m => HasCRUD Person (Mock m) where
  select uid =
    IdMap.lookup uid <$> use store_person

  insert p =
    stating store_person (IdMap.insertNew p)

  update uid p =
    modifying store_person (IdMap.insert uid p)

  delete uid =
    modifying store_person (IdMap.delete uid)

instance MonadIO m => HasCRUD Unit (Mock m) where
  select uid =
    IdMap.lookup uid <$> use store_unit

  insert u =
    stating store_unit (IdMap.insertNew u)

  update uid u =
    modifying store_unit (IdMap.insert uid u)

  delete uid =
    modifying store_unit (IdMap.delete uid)

instance MonadIO m => HasRelation Member (Mock m) where
  selectRelationL uid = do
    let cond (Member u _) = u == uid
    IdMap.filter cond <$> use store_member

  selectRelationR uid = do
    let cond (Member _ u) = u == uid
    IdMap.filter cond <$> use store_member

  insertRelation m =
    stating store_member (IdMap.insertNew m)

  deleteRelation uid =
    modifying store_member (IdMap.delete uid)

instance MonadIO m => HasRelation Subpart (Mock m) where

  selectRelationL uid = do
    let cond (Subpart u _) = u == uid
    IdMap.filter cond <$> use store_subpart

  selectRelationR uid = do
    let cond (Subpart _ u) = u == uid
    IdMap.filter cond <$> use store_subpart

  insertRelation s =
    stating store_subpart (IdMap.insertNew s)

  deleteRelation uid =
    modifying store_subpart (IdMap.delete uid)

--------------------------------------------------------------------------------
-- Helper

stating :: MonadState s m => Lens' s a -> (a -> (x,a)) -> m x
stating l f = do
  let f' s =
        let (x, a) = f (view l s)
        in (x, set l a s)
  state f'
{-# INLINE stating #-}
