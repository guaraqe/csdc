{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module CSDC.Types.GUI
  ( -- GUI Types
    NewUser (..),
    PersonMember (..),
    PersonInfo (..),
    UnitMember (..),
    UnitSubpart (..),
    UnitInfo (..),
    Inbox (..),
    MessageInfo (..),
    ReplyInfo (..),
    MailInvitation (..),
    -- Search
    SearchId (..),
    SearchResult (..),
  )
where

import CSDC.Types.DAO
import CSDC.Types.Id
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- GUI Types

data NewUser = NewUser
  { name :: Text,
    email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data PersonMember = PersonMember
  { memberId :: Id Member,
    unitId :: Id Unit,
    unit :: Unit
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data PersonInfo = PersonInfo
  { id :: Id Person,
    person :: Person,
    members :: [PersonMember],
    unitsForMessage :: [WithId Unit]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data UnitMember = UnitMember
  { memberId :: Id Member,
    personId :: Id Person,
    person :: Person
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data UnitSubpart = UnitSubpart
  { subpartId :: Id Subpart,
    level :: Int,
    unitId :: Id Unit,
    unit :: Unit
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data UnitInfo = UnitInfo
  { id :: Id Unit,
    unit :: Unit,
    members :: [UnitMember],
    children :: [UnitSubpart],
    parents :: [UnitSubpart],
    userId :: Id Person,
    isMember :: Bool,
    isAdmin :: Bool,
    isMembershipPending :: Bool,
    unitsForMessage :: [WithId Unit]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data MessageInfo a = MessageInfo
  { id :: Id (Message a),
    messageType :: MessageType,
    status :: MessageStatus,
    text :: Text,
    value :: a,
    left :: Text,
    right :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data ReplyInfo a = ReplyInfo
  { id :: Id (Reply a),
    replyType :: ReplyType,
    messageType :: MessageType,
    text :: Text,
    status :: ReplyStatus,
    message :: MessageInfo a
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Inbox = Inbox
  { messageMember :: [MessageInfo NewMember],
    replyMember :: [ReplyInfo NewMember],
    messageSubpart :: [MessageInfo NewSubpart],
    replySubpart :: [ReplyInfo NewSubpart]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

instance Semigroup Inbox where
  Inbox mm1 rm1 ms1 rs1 <> Inbox mm2 rm2 ms2 rs2 =
    Inbox (mm1 <> mm2) (rm1 <> rm2) (ms1 <> ms2) (rs1 <> rs2)

instance Monoid Inbox where
  mempty = Inbox [] [] [] []

data MailInvitation = MailInvitation
  { message :: Text,
    invitees :: [Text]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Search

data SearchId = SearchPerson (Id Person) | SearchUnit (Id Unit)
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data SearchResult a = SearchResult
  { id :: a,
    name :: Text
  }
  deriving (Show, Eq, Generic, Functor)
  deriving (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Forum

data NewThread = NewThread
  { unitId :: Id Unit,
    authorId :: Id Person,
    subject :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Thread = Thread
  { unitId :: Id Unit,
    personId :: Id Unit,
    subject :: Text,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data ThreadSummary = ThreadSummary
  { id :: Id Thread,
    unitId :: Id Unit,
    authorId :: Id Unit,
    authorName :: Text,
    subject :: Text,
    createdAt :: POSIXTime,
    last :: POSIXTime,
    messages :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data ThreadInfo = ThreadInfo
  { summary :: ThreadSummary,
    messages :: [WithId ThreadMessage]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data ThreadMessage = ThreadMessage
  { authorId :: Id Person,
    authorName :: Text,
    message :: Text,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
