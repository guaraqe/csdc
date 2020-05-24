{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module CSDC.DAO.Mock
  ( Mock
  , runMock
  , Store
  , makeEmptyStore
  ) where

import CSDC.Data.Id (Id (..), WithId (..))
import CSDC.Data.IdMap (IdMap')
import CSDC.Data.RIO (RIO, runRIO)
import CSDC.DAO.Types
  ( Person (..)
  , Unit (..)
  , Member (..)
  , Subpart (..)
  , Message (..)
  , MessageStatus (..)
  , Reply (..)
  , ReplyStatus (..)
  , ReplyType (..)
  , Inbox (..)
  )
import CSDC.DAO.Class
  ( HasDAO (..)
  , HasCRUD (..)
  , HasRelation (..)
  , HasMessage (..)
  )

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.Data.IdMap as IdMap

import Control.Monad.State.Strict (MonadState (..), modify')
import Control.Monad.IO.Class (MonadIO (..))
import Control.Lens (Lens', makeLenses, view, set, use, over)
import Data.IORef (IORef, newIORef)

--------------------------------------------------------------------------------
-- In-memory store

data Store = Store
  { _store_person :: IdMap' Person
  , _store_unit :: IdMap' Unit
  , _store_member :: IdMap' Member
  , _store_subpart :: IdMap' Subpart
  , _store_messageMember :: IdMap' (Message Member)
  , _store_messageSubpart :: IdMap' (Message Subpart)
  , _store_replyMember :: IdMap' (Reply Member)
  , _store_replySubpart :: IdMap' (Reply Subpart)
  , _store_root :: Id Unit
  } deriving (Show, Eq)

makeLenses ''Store

makeEmptyStore :: MonadIO m => m (IORef Store)
makeEmptyStore = liftIO $ newIORef
  Store
    { _store_person = singleton personId person
    , _store_unit = singleton unitId unit
    , _store_member = singleton memberId member
    , _store_subpart = IdMap.empty
    , _store_messageMember = IdMap.empty
    , _store_messageSubpart = IdMap.empty
    , _store_replyMember = IdMap.empty
    , _store_replySubpart = IdMap.empty
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
  deriving newtype (Functor, Applicative, Monad, MonadState Store, MonadIO)

runMock :: MonadIO m => IORef Store -> Mock m a -> m a
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

  inboxPerson personId = do
    let
      predMessageId m =
        member_person (message_value m) == personId

      predMessageWaiting m =
        message_status m == Waiting

    messageMemberAll <- IdMap.filter predMessageId <$> use store_messageMember

    let
      predReply r =
        reply_id r `elem` IdMap.keys messageMemberAll &&
        reply_status r == NotSeen

      messageMember = IdMap.filter predMessageWaiting messageMemberAll

    replyMember <- IdMap.filter predReply <$> use store_replyMember

    pure Inbox
      { inbox_messageMember = messageMember
      , inbox_replyMember = replyMember
      , inbox_messageSubpart = IdMap.empty
      , inbox_replySubpart = IdMap.empty
      }

  inboxUnit unitId = do
    let
      predMessageId m =
        member_unit (message_value m) == unitId

      predMessageWaiting m =
        message_status m == Waiting

    messageMemberAll <- IdMap.filter predMessageId <$> use store_messageMember

    let
      predReply r =
        reply_id r `elem` IdMap.keys messageMemberAll &&
        reply_status r == NotSeen

      messageMember = IdMap.filter predMessageWaiting messageMemberAll

    replyMember <- IdMap.filter predReply <$> use store_replyMember

    pure Inbox
      { inbox_messageMember = messageMember
      , inbox_replyMember = replyMember
      , inbox_messageSubpart = IdMap.empty
      , inbox_replySubpart = IdMap.empty
      }

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

instance MonadIO m => HasMessage Member (Mock m ) where
  sendMessage m =
    stating store_messageMember (IdMap.insertNew m)

  sendReply r = do
    rid <- stating store_replyMember (IdMap.insertNew r)
    let
      status = case reply_type r of
        Accept -> Accepted
        Reject -> Rejected
      uid = reply_id r
    modifying store_messageMember $ IdMap.update uid (markMessage status)
    pure rid

  viewReply uid =
    modifying store_replyMember $ IdMap.update uid (markReply Seen)

instance MonadIO m => HasMessage Subpart (Mock m ) where
  sendMessage m = do
    stating store_messageSubpart (IdMap.insertNew m)

  sendReply r = do
    rid <- stating store_replySubpart (IdMap.insertNew r)
    let
      status = case reply_type r of
        Accept -> Accepted
        Reject -> Rejected
      uid = reply_id r
    modifying store_messageSubpart $ IdMap.update uid (markMessage status)
    pure rid

  viewReply uid =
    modifying store_replySubpart $ IdMap.update uid (markReply Seen)

--------------------------------------------------------------------------------
-- Helper

stating :: MonadState s m => Lens' s a -> (a -> (x,a)) -> m x
stating l f = do
  let f' !s =
        let !(!x, !a) = f (view l s)
        in (x, set l a s)
  state f'
{-# INLINE stating #-}

modifying :: MonadState s m => Lens' s a -> (a -> a) -> m ()
modifying l f = modify' (over l f)
{-# INLINE modifying #-}

markMessage :: MessageStatus -> Message a -> Message a
markMessage s m = m { message_status = s }

markReply :: ReplyStatus -> Reply a -> Reply a
markReply s r = r { reply_status = s }
