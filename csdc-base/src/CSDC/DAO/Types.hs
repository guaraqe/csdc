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
  , Reply (..)
  , ReplyStatus (..)
  , Inbox (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Data.Id (Id)
import CSDC.Data.IdMap (IdMap')

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

data Message a
  = Invitation a Text MessageStatus
  | Submission a Text MessageStatus

data ReplyStatus = Seen | NotSeen

data Reply a
  = Accept (Id (Message a)) Text ReplyStatus
  | Reject (Id (Message a)) Text ReplyStatus

data Inbox = Inbox
  { inbox_messageMember :: IdMap' (Message Member)
  , inbox_replyMember :: IdMap' (Reply Member)
  , inbox_messageSubpart :: IdMap' (Message Subpart)
  , inbox_replySubpart :: IdMap' (Reply Subpart)
  }
