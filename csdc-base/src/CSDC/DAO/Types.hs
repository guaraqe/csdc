{-# LANGUAGE StrictData #-}

module CSDC.DAO.Types
  ( -- Entities
    Person (..)
  , Unit (..)
    -- Relations
  , Member (..)
  , Subpart (..)
    -- Messages
  , Message (..)
  , MessageStatus (..)
  , MessageType (..)
  , Reply (..)
  , ReplyStatus (..)
  , ReplyType (..)
    -- GUI Types
  , PersonInfo (..)
  , UnitInfo (..)
  , Inbox (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Data.Id (Id, WithId)
import CSDC.Data.IdMap (IdMap, IdMap')

import qualified CSDC.Auth.ORCID as ORCID

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name :: Text
  , person_description :: Text
  , person_orcid :: ORCID.Id
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Person

data Unit = Unit
  { unit_name :: Text
  , unit_description :: Text
  , unit_chair :: Id Member
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Unit

--------------------------------------------------------------------------------
-- Fundamental relations

data Member = Member
  { member_person :: Id Person
  , member_unit :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Member

data Subpart = Subpart
  { subpart_child :: Id Unit
  , subpart_parent :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Subpart

--------------------------------------------------------------------------------
-- Messages

data MessageStatus = Waiting | Accepted | Rejected
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON MessageStatus

data MessageType = Invitation | Submission
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON MessageType

data Message a = Message
  { message_type :: MessageType
  , message_text :: Text
  , message_status :: MessageStatus
  , message_value :: a
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Message a)

data ReplyStatus = Seen | NotSeen
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON ReplyStatus

data ReplyType = Accept | Reject
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON ReplyType

data Reply a = Reply
  { reply_type :: ReplyType
  , reply_mtype :: MessageType
  , reply_text :: Text
  , reply_status :: ReplyStatus
  , reply_id :: Id (Message a)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Reply a)

--------------------------------------------------------------------------------
-- GUI Types

data PersonInfo = PersonInfo
  { personInfo_id :: Id Person
  , personInfo_person :: Person
  , personInfo_members :: IdMap Member (WithId Unit)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonInfo

data UnitInfo = UnitInfo
  { unitInfo_id :: Id Unit
  , unitInfo_unit :: Unit
  , unitInfo_members :: IdMap Member (WithId Person)
  , unitInfo_children :: IdMap Subpart (WithId Unit)
  , unitInfo_parents :: IdMap Subpart (WithId Unit)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON UnitInfo

data Inbox = Inbox
  { inbox_messageMember :: IdMap' (Message Member)
  , inbox_replyMember :: IdMap' (Reply Member)
  , inbox_messageSubpart :: IdMap' (Message Subpart)
  , inbox_replySubpart :: IdMap' (Reply Subpart)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Inbox
