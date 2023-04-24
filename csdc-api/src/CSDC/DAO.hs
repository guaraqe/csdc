{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.DAO where

import CSDC.Action
import CSDC.Image
import CSDC.Mail qualified as Mail
import CSDC.Mail.Templates qualified as Mail.Templates
import CSDC.Prelude
import CSDC.SQL.Elections qualified as SQL.Elections
import CSDC.SQL.Files qualified as SQL.Files
import CSDC.SQL.Forum qualified as SQL.Forum
import CSDC.SQL.Mail qualified as SQL.Mail
import CSDC.SQL.MailInvitations qualified as SQL.MailInvitations
import CSDC.SQL.Members qualified as SQL.Members
import CSDC.SQL.MessageMembers qualified as SQL.MessageMembers
import CSDC.SQL.MessageSubparts qualified as SQL.MessageSubparts
import CSDC.SQL.Persons qualified as SQL.Persons
import CSDC.SQL.Subparts qualified as SQL.Subparts
import CSDC.SQL.Units qualified as SQL.Units
import CSDC.Types.Election
import CSDC.Types.File
import Control.Monad (forM_)
import Control.Monad.Reader (asks)
import Data.Password.Bcrypt (hashPassword, mkPassword)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath

--------------------------------------------------------------------------------
-- User

createUser :: NewUser -> Action user (Id Person)
createUser newUser@(NewUser {..}) = do
  passwordHash <- hashPassword $ mkPassword password
  let person =
        NewPerson
          { name = name,
            email = email,
            password = passwordHash,
            description = "",
            image = ""
          }
  pid <- insertPerson person
  imageBS <- liftIO $ generateImageFromName name
  let image = base64FileFromByteString "profile.svg" imageBS
  updatePersonImage pid image

  -- Send confirmation e-mail
  sendMail $ Mail.Templates.confirmation newUser

  -- Check for invitations
  uids <- runQuery SQL.MailInvitations.select email
  forM_ uids $ \uid ->
    insertMember $ NewMember pid uid
  runQuery SQL.MailInvitations.delete email

  pure pid

getUser :: ActionAuth (Id Person)
getUser = asks (.user)

--------------------------------------------------------------------------------
-- Person

getPerson :: Id Person -> Action user Person
getPerson pid =
  runQuery SQL.Persons.select pid >>= \case
    Nothing -> throw $ PersonDoesNotExist pid
    Just unit -> pure unit

selectPerson :: Id Person -> Action user (Maybe Person)
selectPerson i = runQuery SQL.Persons.select i

insertPerson :: NewPerson -> Action user (Id (Person))
insertPerson p = runQuery SQL.Persons.insert p

updatePerson :: Id Person -> PersonUpdate -> Action user ()
updatePerson i p = do
  runQuery SQL.Persons.update (i, p)

updatePersonImage :: Id Person -> Base64File -> Action user ()
updatePersonImage i image = do
  imagePath <- base64FileToPath "person" i image
  runQuery SQL.Persons.updateImage (i, imagePath)

deletePerson :: Id Person -> Action user ()
deletePerson i = runQuery SQL.Persons.delete i

--------------------------------------------------------------------------------
-- Unit

getUnit :: Id Unit -> Action user Unit
getUnit uid =
  runQuery SQL.Units.select uid >>= \case
    Nothing -> throw $ UnitDoesNotExist uid
    Just unit -> pure unit

selectUnit :: Id Unit -> Action user (Maybe Unit)
selectUnit i = runQuery SQL.Units.select i

insertUnit :: NewUnit -> ActionAuth (Id (Unit))
insertUnit u = do
  user <- getUser
  now <- liftIO getPOSIXTime
  let unit =
        Unit
          { name = u.name,
            description = u.description,
            chairId = user,
            image = "", -- will be updated later
            createdAt = now -- will be ignored
          }
  uid <- runQuery SQL.Units.insert unit
  imageBS <- liftIO $ generateImageFromName u.name
  let image = base64FileFromByteString "profile.svg" imageBS
  updateUnitImage uid image
  pure uid

updateUnit :: Id Unit -> UnitUpdate -> Action user ()
updateUnit i p = runQuery SQL.Units.update (i, p)

updateUnitImage :: Id Unit -> Base64File -> Action user ()
updateUnitImage i image = do
  imagePath <- base64FileToPath "unit" i image
  runQuery SQL.Units.updateImage (i, imagePath)

deleteUnit :: Id Unit -> Action user ()
deleteUnit i = do
  runQuery SQL.Subparts.deleteUnit i
  runQuery SQL.Members.deleteUnit i
  runQuery SQL.Units.delete i

changeUnitChair :: Id Unit -> Id Person -> Action user ()
changeUnitChair uid pid =
  runQuery SQL.Units.updateChair (uid, pid)

sendMailInvitation :: Id Unit -> MailInvitation -> Action user ()
sendMailInvitation unitId MailInvitation {..} = do
  unit <- getUnit unitId
  chair <- getPerson unit.chairId
  forM_ invitees $ \invitee -> do
    sendMail $ Mail.Templates.invitation unit chair message invitee
    runQuery SQL.MailInvitations.insert (unitId, invitee)

--------------------------------------------------------------------------------
-- Member

insertMember :: NewMember -> Action user (Id Member)
insertMember r = runQuery SQL.Members.insert r

deleteMember :: Id Member -> Action user ()
deleteMember i = runQuery SQL.Members.delete i

--------------------------------------------------------------------------------
-- Subpart

insertSubpart :: NewSubpart -> Action user (Id Subpart)
insertSubpart r = runQuery SQL.Subparts.insert r

deleteSubpart :: Id Subpart -> Action user ()
deleteSubpart i = runQuery SQL.Subparts.delete i

--------------------------------------------------------------------------------
-- Message NewMember

sendMessageMember :: NewMessage NewMember -> Action user (Id (Message NewMember))
sendMessageMember m = runQuery SQL.MessageMembers.sendMessage m

sendReplyMember :: NewReply NewMember -> Action user (Id (Reply NewMember))
sendReplyMember r = do
  rid <- runQuery SQL.MessageMembers.sendReply r
  let status = case r.replyType of
        Accept -> Accepted
        Reject -> Rejected
      uid = r.messageId
  runQuery SQL.MessageMembers.updateMessage (uid, status)
  case r.replyType of
    Accept ->
      runQuery SQL.MessageMembers.selectMember uid >>= \case
        Nothing ->
          pure ()
        Just val -> do
          _ <- insertMember val
          pure ()
    _ ->
      pure ()
  pure rid

viewReplyMember :: Id (Reply NewMember) -> Action user ()
viewReplyMember i = runQuery SQL.MessageMembers.viewReply i

--------------------------------------------------------------------------------
-- Message NewSubpart

sendMessageSubpart :: NewMessage NewSubpart -> Action user (Id (Message NewSubpart))
sendMessageSubpart m = runQuery SQL.MessageSubparts.sendMessage m

sendReplySubpart :: NewReply NewSubpart -> Action user (Id (Reply NewSubpart))
sendReplySubpart r = do
  rid <- runQuery SQL.MessageSubparts.sendReply r
  let status = case r.replyType of
        Accept -> Accepted
        Reject -> Rejected
      uid = r.messageId
  runQuery SQL.MessageSubparts.updateMessage (uid, status)
  case r.replyType of
    Accept ->
      runQuery SQL.MessageSubparts.selectSubpart uid >>= \case
        Nothing ->
          pure ()
        Just val -> do
          _ <- insertSubpart val
          pure ()
    _ ->
      pure ()
  pure rid

viewReplySubpart :: Id (Reply NewSubpart) -> Action user ()
viewReplySubpart i = runQuery SQL.MessageSubparts.viewReply i

--------------------------------------------------------------------------------
-- Others

createUnit :: NewUnit -> ActionAuth (Id Unit)
createUnit unit = do
  pid <- getUser
  uid <- insertUnit unit
  let member =
        NewMember
          { personId = pid,
            unitId = uid
          }
  _ <- insertMember member
  return uid

getUserInbox :: ActionAuth Inbox
getUserInbox = do
  pid <- getUser
  messageMembers <-
    runQuery SQL.MessageMembers.selectMessages $
      SQL.MessageMembers.Filter (Just pid) Nothing

  replyMembers <-
    runQuery SQL.MessageMembers.selectReplies $
      SQL.MessageMembers.Filter (Just pid) Nothing

  let userInbox =
        Inbox
          { messageMember = messageMembers,
            replyMember = replyMembers,
            messageSubpart = [],
            replySubpart = []
          }

  units <- getUnitsWhoseChairIsUser

  unitInboxes <- mapM (getUnitInbox . (.id)) units

  pure $ mconcat (userInbox : unitInboxes)

getUnitInbox :: Id Unit -> Action user Inbox
getUnitInbox uid = do
  messageSubparts <-
    runQuery SQL.MessageSubparts.selectMessagesForUnit uid

  replySubparts <-
    runQuery SQL.MessageSubparts.selectRepliesForUnit uid

  messageMembers <-
    runQuery SQL.MessageMembers.selectMessages $
      SQL.MessageMembers.Filter Nothing (Just uid)

  replyMembers <-
    runQuery SQL.MessageMembers.selectReplies $
      SQL.MessageMembers.Filter Nothing (Just uid)

  pure
    Inbox
      { messageMember = messageMembers,
        replyMember = replyMembers,
        messageSubpart = messageSubparts,
        replySubpart = replySubparts
      }

getUserInfo :: ActionAuth (Maybe PersonInfo)
getUserInfo = do
  uid <- getUser
  getPersonInfo uid

getPersonInfo :: Id Person -> ActionAuth (Maybe PersonInfo)
getPersonInfo uid =
  selectPerson uid >>= \case
    Nothing -> pure Nothing
    Just person -> do
      userId <- getUser
      members <- runQuery SQL.Members.selectByPerson uid
      unitsForMessage <-
        runQuery SQL.MessageMembers.getUnitsForMessage (userId, uid)
      pure $
        Just
          PersonInfo
            { id = uid,
              person = person,
              members = members,
              unitsForMessage = unitsForMessage
            }

getUnitMembers :: Id Unit -> Action user [UnitMember]
getUnitMembers uid = runQuery SQL.Members.selectByUnit uid

getUnitChildren :: Id Unit -> Action user [UnitSubpart]
getUnitChildren uid = runQuery SQL.Subparts.selectByParent uid

getUnitParents :: Id Unit -> Action user [UnitSubpart]
getUnitParents uid = runQuery SQL.Subparts.selectByChild uid

getUnitInfo :: Id Unit -> ActionAuth (Maybe UnitInfo)
getUnitInfo uid = do
  userId <- getUser
  isMembershipPending <-
    runQuery SQL.MessageMembers.isMembershipPending (userId, uid)
  selectUnit uid >>= \case
    Nothing -> pure Nothing
    Just unit -> do
      members <- getUnitMembers uid
      children <- getUnitChildren uid
      parents <- getUnitParents uid
      unitsForMessage <-
        runQuery SQL.MessageSubparts.getUnitsForMessage (userId, uid)
      let isMember = any (\m -> m.personId == userId) members
      isIndirectMember <-
        if False --isMember
          then pure True
          else runQuery SQL.Members.isIndirectMember (userId, uid)
      pure $
        Just
          UnitInfo
            { id = uid,
              unit = unit,
              members = members,
              children = children,
              parents = parents,
              userId = userId,
              isAdmin = unit.chairId == userId,
              isMember = isMember,
              isIndirectMember = isIndirectMember,
              isMembershipPending = isMembershipPending,
              unitsForMessage = unitsForMessage
            }

getUnitsWhoseChairIsUser :: ActionAuth [WithId Unit]
getUnitsWhoseChairIsUser = do
  uid <- getUser
  runQuery SQL.Units.selectByChair uid

--------------------------------------------------------------------------------
-- Search

searchUnits :: Text -> Action user [WithId Unit]
searchUnits query = do
  let parts = Text.words query
      toPattern part = "%" <> part <> "%"
  runQuery SQL.Units.searchUnits $ fmap toPattern parts

searchAll :: Text -> Action user [SearchResult SearchId]
searchAll query = do
  let parts = Text.words query
      toPattern part = "%" <> part <> "%"
  persons <- runQuery SQL.Persons.search $ fmap toPattern parts
  units <- runQuery SQL.Units.search $ fmap toPattern parts
  pure $ fmap (fmap SearchPerson) persons <> fmap (fmap SearchUnit) units

--------------------------------------------------------------------------------
-- Files

profileImageName :: Text -> Text
profileImageName name =
  let (_, ext) = splitExtension (Text.unpack name)
   in Text.pack $ "photo" <> ext

base64FileToPath :: Text -> Id a -> Base64File -> Action user Text
base64FileToPath folder (Id i) image = do
  let fileName = profileImageName image.name
      fileFolder = folder <> "/" <> Text.pack (show i)
      imagePath = fileFolder <> "/" <> fileName
      newImage = image {name = fileName} :: Base64File
  filedb <- toNewFileDB fileFolder $ fromBase64File newImage
  runQuery SQL.Files.upsertFile filedb
  return imagePath

insertUnitFile :: Id Unit -> File -> Action user ()
insertUnitFile i file = do
  let fileFolder = "unit" <> "/" <> Text.pack (show i)
  filedb <- toNewFileDB fileFolder file
  runQuery SQL.Files.upsertFile filedb

getUnitFiles :: Id Unit -> Action user [FileUI]
getUnitFiles i = do
  let fileFolder = "unit" <> "/" <> Text.pack (show i)
  filesDB <- runQuery SQL.Files.selectFolderFiles fileFolder
  let toFileUI FileDB {..} =
        FileUI
          { path = folder <> "/" <> name,
            name = name,
            size = size,
            modifiedAt = modifiedAt
          }
  pure $ fmap toFileUI filesDB

--------------------------------------------------------------------------------
-- Forum

createThread :: Id Unit -> NewThread -> ActionAuth (Id Thread)
createThread uid NewThread {..} = do
  user <- getUser
  let thread =
        Thread
          { unitId = uid,
            authorId = user,
            subject = subject
          }
  tid <- runQuery SQL.Forum.insertThread thread

  let post =
        Post
          { threadId = tid,
            authorId = user,
            text = text
          }
  _ <- runQuery SQL.Forum.insertPost post

  pure tid

getThreads :: Id Unit -> ActionAuth [ThreadInfo]
getThreads uid = runQuery SQL.Forum.selectThreads uid

createPost :: Id Thread -> NewPost -> ActionAuth (Id Post)
createPost tid NewPost {..} = do
  user <- getUser
  let post =
        Post
          { threadId = tid,
            authorId = user,
            text = text
          }
  runQuery SQL.Forum.insertPost post

getPosts :: Id Thread -> ActionAuth [PostInfo]
getPosts tid = runQuery SQL.Forum.selectPosts tid

--------------------------------------------------------------------------------
-- Elections

createElection :: Id Unit -> NewElection -> ActionAuth (Id Election)
createElection unitId newElection = do
  runQuery SQL.Elections.insertElection (unitId, newElection)

getElections :: Id Unit -> ActionAuth [ElectionInfo]
getElections unitId = do
  personId <- getUser
  runQuery SQL.Elections.selectElections (unitId, personId)

deleteElection :: Id Election -> ActionAuth ()
deleteElection electionId = do
  runQuery SQL.Elections.deleteElection electionId

addVote :: Id Election -> VotePayload -> ActionAuth (Id Vote)
addVote electionId payload = do
  personId <- getUser
  runQuery SQL.Elections.insertVoter (electionId, personId)
  runQuery SQL.Elections.insertVote (electionId, payload)

--------------------------------------------------------------------------------
-- Mail

sendMail :: Mail.Mail -> Action user ()
sendMail = runQuery SQL.Mail.insert
