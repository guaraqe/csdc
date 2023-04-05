{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CSDC.API.DAO
  ( API,
    NamedAPI,
    serveAPI,
  )
where

import CSDC.Action
import CSDC.DAO
import CSDC.Prelude
import CSDC.Types.Election (Election, ElectionInfo, NewElection, NewVote, Vote)
import CSDC.Types.File (Base64File, File (..), FileUI)
import Data.ByteString.Lazy qualified as Lazy
import Servant hiding (Post)
import Servant qualified
import Servant.Multipart

--------------------------------------------------------------------------------
-- Synonyms

type GetJSON a = Get '[JSON] a

type PostJSON a b = ReqBody '[JSON] a :> Servant.Post '[JSON] b

type DeleteJSON a = Delete '[JSON] a

type CaptureId a = Capture "id" (Id a)

--------------------------------------------------------------------------------
-- User API

data UserAPI mode = UserAPI
  { getUserInfo :: mode :- "info" :> GetJSON (Maybe PersonInfo),
    getUserInbox :: mode :- "inbox" :> GetJSON Inbox,
    getUnitsWhoseChairIsUser :: mode :- "units" :> GetJSON [WithId Unit]
  }
  deriving (Generic)

userAPI :: ServerAuth UserAPI
userAPI =
  UserAPI
    { getUserInfo,
      getUserInbox,
      getUnitsWhoseChairIsUser
    }

--------------------------------------------------------------------------------
-- Person API

data PersonAPI mode = PersonAPI
  { updatePerson :: mode :- CaptureId Person :> PostJSON PersonUpdate (),
    updatePersonImage :: mode :- CaptureId Person :> "image" :> PostJSON Base64File (),
    getPersonInfo :: mode :- CaptureId Person :> "info" :> GetJSON (Maybe PersonInfo)
  }
  deriving (Generic)

personAPI :: ServerAuth PersonAPI
personAPI =
  PersonAPI
    { updatePerson,
      updatePersonImage,
      getPersonInfo
    }

--------------------------------------------------------------------------------
-- Unit API

data UnitAPI mode = UnitAPI
  { createUnit :: mode :- PostJSON NewUnit (Id Unit),
    selectUnit :: mode :- CaptureId Unit :> GetJSON (Maybe Unit),
    updateUnit :: mode :- CaptureId Unit :> PostJSON UnitUpdate (),
    deleteUnit :: mode :- CaptureId Unit :> DeleteJSON (),
    updateUnitImage :: mode :- CaptureId Unit :> "image" :> PostJSON Base64File (),
    getUnitInfo :: mode :- CaptureId Unit :> "info" :> GetJSON (Maybe UnitInfo),
    getUnitChildren :: mode :- CaptureId Unit :> "children" :> GetJSON [UnitSubpart],
    getUnitParents :: mode :- CaptureId Unit :> "parents" :> GetJSON [UnitSubpart],
    changeUnitChair :: mode :- CaptureId Unit :> "chair" :> PostJSON (Id Person) (),
    getUnitFiles :: mode :- CaptureId Unit :> "files" :> GetJSON [FileUI],
    insertUnitFile :: mode :- CaptureId Unit :> "files" :> MultipartForm Mem File :> Servant.Post '[JSON] (),
    sendMailInvitation :: mode :- CaptureId Unit :> "invitation" :> PostJSON MailInvitation ()
  }
  deriving (Generic)

unitAPI :: ServerAuth UnitAPI
unitAPI =
  UnitAPI
    { createUnit,
      selectUnit,
      updateUnit,
      deleteUnit,
      updateUnitImage,
      getUnitInfo,
      getUnitChildren,
      getUnitParents,
      changeUnitChair,
      getUnitFiles,
      insertUnitFile,
      sendMailInvitation
    }

instance FromMultipart Mem File where
  fromMultipart parts = do
    fileData <- lookupFile "file" parts
    pure
      File
        { name = fdFileName fileData,
          contents = Lazy.toStrict $ fdPayload fileData
        }

--------------------------------------------------------------------------------
-- Elections

data ElectionAPI mode = ElectionAPI
  { -- POST election/unit/<unit-uuid>/
    createElection :: mode :- "unit" :> CaptureId Unit :> PostJSON NewElection (Id Election),
    -- GET election/unit/<unit-uuid>/
    getElections :: mode :- "unit" :> CaptureId Unit :> GetJSON [ElectionInfo],
    -- DELETE election/<election-id>
    deleteElection :: mode :- CaptureId Election :> DeleteJSON (),
    -- POST election/<election-id>/vote
    addVote :: mode :- CaptureId Election :> "vote" :> PostJSON NewVote (Id Vote)
  }
  deriving (Generic)

electionAPI :: ServerAuth ElectionAPI
electionAPI =
  ElectionAPI
    { createElection,
      getElections,
      deleteElection,
      addVote
    }

--------------------------------------------------------------------------------
-- Member API

data MemberAPI mode = MemberAPI
  { insertMember :: mode :- PostJSON NewMember (Id Member),
    deleteMember :: mode :- CaptureId Member :> DeleteJSON ()
  }
  deriving (Generic)

memberAPI :: ServerAuth MemberAPI
memberAPI =
  MemberAPI
    { insertMember,
      deleteMember
    }

--------------------------------------------------------------------------------
-- Subpart API

data SubpartAPI mode = SubpartAPI
  { insertSubpart :: mode :- PostJSON NewSubpart (Id Subpart),
    deleteSubpart :: mode :- CaptureId Subpart :> DeleteJSON ()
  }
  deriving (Generic)

subpartAPI :: ServerAuth SubpartAPI
subpartAPI =
  SubpartAPI
    { insertSubpart,
      deleteSubpart
    }

--------------------------------------------------------------------------------
-- Message API

data MessageMemberAPI mode = MessageMemberAPI
  { sendMessageMember :: mode :- "send" :> PostJSON (NewMessage NewMember) (Id (Message NewMember)),
    sendReplyMember :: mode :- "reply" :> PostJSON (NewReply NewMember) (Id (Reply NewMember)),
    viewReplyMember :: mode :- "view" :> PostJSON (Id (Reply NewMember)) ()
  }
  deriving (Generic)

messageMemberAPI :: ServerAuth MessageMemberAPI
messageMemberAPI =
  MessageMemberAPI
    { sendMessageMember,
      sendReplyMember,
      viewReplyMember
    }

data MessageSubpartAPI mode = MessageSubpartAPI
  { sendMessageSubpart :: mode :- "send" :> PostJSON (NewMessage NewSubpart) (Id (Message NewSubpart)),
    sendReplySubpart :: mode :- "reply" :> PostJSON (NewReply NewSubpart) (Id (Reply NewSubpart)),
    viewReplySubpart :: mode :- "view" :> PostJSON (Id (Reply NewSubpart)) ()
  }
  deriving (Generic)

messageSubpartAPI :: ServerAuth MessageSubpartAPI
messageSubpartAPI =
  MessageSubpartAPI
    { sendMessageSubpart,
      sendReplySubpart,
      viewReplySubpart
    }

data MessageAPI mode = MessageAPI
  { messageMemberAPI :: mode :- "member" :> NamedRoutes MessageMemberAPI,
    messageSubpartAPI :: mode :- "subpart" :> NamedRoutes MessageSubpartAPI,
    getUnitInbox :: mode :- "inbox" :> "unit" :> CaptureId Unit :> GetJSON Inbox
  }
  deriving (Generic)

messageAPI :: ServerAuth MessageAPI
messageAPI =
  MessageAPI
    { messageMemberAPI,
      messageSubpartAPI,
      getUnitInbox
    }

--------------------------------------------------------------------------------
-- Search

data SearchAPI mode = SearchAPI
  { searchUnits :: mode :- "units" :> Capture "query" Text :> GetJSON [WithId Unit],
    searchAll :: mode :- "all" :> Capture "query" Text :> GetJSON [SearchResult SearchId]
  }
  deriving (Generic)

searchAPI :: ServerAuth (SearchAPI)
searchAPI =
  SearchAPI
    { searchUnits,
      searchAll
    }

--------------------------------------------------------------------------------
-- Forum

data ForumAPI mode = ForumAPI
  { createThread :: mode :- "unit" :> CaptureId Unit :> PostJSON NewThread (Id Thread),
    getThreads :: mode :- "unit" :> CaptureId Unit :> GetJSON [ThreadInfo],
    createPost :: mode :- "thread" :> CaptureId Thread :> PostJSON NewPost (Id Post),
    getPosts :: mode :- "thread" :> CaptureId Thread :> GetJSON [PostInfo]
  }
  deriving (Generic)

forumAPI :: ServerAuth ForumAPI
forumAPI =
  ForumAPI
    { createThread,
      getThreads,
      createPost,
      getPosts
    }

--------------------------------------------------------------------------------
-- API

type API = NamedRoutes NamedAPI

data NamedAPI mode = NamedAPI
  { userAPI :: mode :- "user" :> NamedRoutes UserAPI,
    personAPI :: mode :- "person" :> NamedRoutes PersonAPI,
    unitAPI :: mode :- "unit" :> NamedRoutes UnitAPI,
    memberAPI :: mode :- "member" :> NamedRoutes MemberAPI,
    subpartAPI :: mode :- "subpart" :> NamedRoutes SubpartAPI,
    messageAPI :: mode :- "message" :> NamedRoutes MessageAPI,
    searchAPI :: mode :- "search" :> NamedRoutes SearchAPI,
    forumAPI :: mode :- "forum" :> NamedRoutes ForumAPI,
    electionAPI :: mode :- "election" :> NamedRoutes ElectionAPI
  }
  deriving (Generic)

serveAPI :: ServerAuth NamedAPI
serveAPI =
  NamedAPI
    { userAPI,
      personAPI,
      unitAPI,
      memberAPI,
      subpartAPI,
      messageAPI,
      searchAPI,
      forumAPI,
      electionAPI
    }
