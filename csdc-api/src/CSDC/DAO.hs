{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module CSDC.DAO where

import CSDC.Action
import CSDC.Image
import CSDC.Prelude
import CSDC.Types.File

import qualified CSDC.IPFS as IPFS
import qualified CSDC.Mail as Mail
import qualified CSDC.Mail.Templates as Mail.Templates
import qualified CSDC.SQL.Files as SQL.Files
import qualified CSDC.SQL.Forum as SQL.Forum
import qualified CSDC.SQL.Mail as SQL.Mail
import qualified CSDC.SQL.MailInvitations as SQL.MailInvitations
import qualified CSDC.SQL.Members as SQL.Members
import qualified CSDC.SQL.MessageMembers as SQL.MessageMembers
import qualified CSDC.SQL.MessageSubparts as SQL.MessageSubparts
import qualified CSDC.SQL.Persons as SQL.Persons
import qualified CSDC.SQL.Subparts as SQL.Subparts
import qualified CSDC.SQL.Units as SQL.Units

import Control.Monad (forM_)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.Password.Bcrypt (mkPassword, hashPassword)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath

import qualified Data.Text as Text
import qualified UnliftIO.Async

--------------------------------------------------------------------------------
-- User

createUser :: NewUser -> Action user (Id Person)
createUser newUser@(NewUser {..}) = do
  password <- hashPassword $ mkPassword newUser_password
  let
    person = NewPerson
      { newPerson_name = newUser_name
      , newPerson_email = newUser_email
      , newPerson_password = password
      , newPerson_description = ""
      , newPerson_image = ""
      }
  pid <- insertPerson person
  imageBS <- liftIO $ generateImageFromName newUser_name
  let image = base64FileFromByteString "profile.svg" imageBS
  updatePersonImage pid image

  -- Send confirmation e-mail
  sendMail $ Mail.Templates.confirmation newUser

  -- Check for invitations
  uids <- runQuery SQL.MailInvitations.select newUser_email
  forM_ uids $ \uid ->
     insertMember $ NewMember pid uid
  runQuery SQL.MailInvitations.delete newUser_email

  pure pid

getUser :: ActionAuth (Id Person)
getUser = asks context_user

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
  runQuery SQL.Persons.update (i,p)

updatePersonImage :: Id Person -> Base64File -> Action user ()
updatePersonImage i image = do
  imagePath <- base64FileToPath "person" i image
  runQuery SQL.Persons.updateImage (i,imagePath)

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
  let unit = Unit
        { unit_name = newUnit_name u
        , unit_description = newUnit_description u
        , unit_chair = user
        , unit_image = "" -- will be updated later
        , unit_createdAt = now -- will be ignored
        }
  uid <- runQuery SQL.Units.insert unit
  imageBS <- liftIO $ generateImageFromName $ newUnit_name u
  let image = base64FileFromByteString "profile.svg" imageBS
  updateUnitImage uid image
  pure uid

updateUnit :: Id Unit -> UnitUpdate -> Action user ()
updateUnit i p = runQuery SQL.Units.update (i,p)

updateUnitImage :: Id Unit -> Base64File -> Action user ()
updateUnitImage i image = do
  imagePath <- base64FileToPath "unit" i image
  runQuery SQL.Units.updateImage (i,imagePath)

deleteUnit :: Id Unit -> Action user ()
deleteUnit i = do
  runQuery SQL.Subparts.deleteUnit i
  runQuery SQL.Members.deleteUnit i
  runQuery SQL.Units.delete i

changeUnitChair :: Id Unit -> Id Person -> Action user ()
changeUnitChair uid pid =
  runQuery SQL.Units.updateChair (uid,pid)

sendMailInvitation :: Id Unit -> MailInvitation -> Action user ()
sendMailInvitation unitId MailInvitation {..} = do
  unit <- getUnit unitId
  chair <- getPerson $ unit_chair unit
  forM_ mailInvitation_invitees $ \invitee -> do
    sendMail $ Mail.Templates.invitation unit chair mailInvitation_message invitee
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
  let
    status = case newReply_type r of
      Accept -> Accepted
      Reject -> Rejected
    uid = newReply_message r
  runQuery SQL.MessageMembers.updateMessage (uid, status)
  case newReply_type r of
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
  let
    status = case newReply_type r of
      Accept -> Accepted
      Reject -> Rejected
    uid = newReply_message r
  runQuery SQL.MessageSubparts.updateMessage (uid, status)
  case newReply_type r of
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
  let member = NewMember
        { newMember_person = pid
        , newMember_unit = uid
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

  let userInbox = Inbox
        { inbox_messageMember = messageMembers
        , inbox_replyMember = replyMembers
        , inbox_messageSubpart = []
        , inbox_replySubpart = []
        }

  units <- getUnitsWhoseChairIsUser

  unitInboxes <- mapM (getUnitInbox . withId_id) units

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

  pure Inbox
    { inbox_messageMember = messageMembers
    , inbox_replyMember = replyMembers
    , inbox_messageSubpart = messageSubparts
    , inbox_replySubpart = replySubparts
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
      pure $ Just PersonInfo
        { personInfo_id = uid
        , personInfo_person = person
        , personInfo_members = members
        , personInfo_unitsForMessage = unitsForMessage
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
      pure $ Just UnitInfo
        { unitInfo_id = uid
        , unitInfo_unit = unit
        , unitInfo_members = members
        , unitInfo_children = children
        , unitInfo_parents = parents
        , unitInfo_user = userId
        , unitInfo_isAdmin = unit_chair unit == userId
        , unitInfo_isMember = any (\m -> unitMember_id m == userId) members
        , unitInfo_isMembershipPending = isMembershipPending
        , unitInfo_unitsForMessage = unitsForMessage
        }

getUnitsWhoseChairIsUser :: ActionAuth [WithId Unit]
getUnitsWhoseChairIsUser = do
  uid <- getUser
  runQuery SQL.Units.selectByChair uid

--------------------------------------------------------------------------------
-- Search

searchUnits :: Text -> Action user [WithId Unit]
searchUnits query = do
  let
    parts = Text.words query
    toPattern part = "%" <> part <> "%"
  runQuery SQL.Units.searchUnits $ fmap toPattern parts

searchAll :: Text -> Action user [SearchResult SearchId]
searchAll query = do
  let
    parts = Text.words query
    toPattern part = "%" <> part <> "%"
  persons <- runQuery SQL.Persons.search $ fmap toPattern parts
  units <- runQuery SQL.Units.search $ fmap toPattern parts
  pure $ fmap (fmap SearchPerson) persons <> fmap (fmap SearchUnit) units

--------------------------------------------------------------------------------
-- Files

profileImageName :: Text -> Text
profileImageName name =
  let
    (_,ext) = splitExtension (Text.unpack name)
  in
    Text.pack $ "photo" <> ext

base64FileToPath :: Text -> Id a -> Base64File -> Action user Text
base64FileToPath folder (Id i) image = do
  let
    fileName = profileImageName (base64File_name image)
    fileFolder = folder <> "/" <> Text.pack (show i)
    imagePath = fileFolder <> "/" <> fileName
    newImage = image { base64File_name = fileName }
  filedb <- toNewFileDB fileFolder $ fromBase64File newImage
  runQuery SQL.Files.upsertFile filedb
  return imagePath

insertUnitFile :: Id Unit -> File -> Action user ()
insertUnitFile i file = do
  let fileFolder = "unit" <> "/" <> Text.pack (show i)
  filedb <- toNewFileDB fileFolder file
  _ <- UnliftIO.Async.async $ saveFileIPFS (newFileDB_hash filedb) file
  runQuery SQL.Files.upsertFile filedb

getUnitFiles :: Id Unit -> Action user [FileUI]
getUnitFiles i = do
  let fileFolder = "unit" <> "/" <> Text.pack (show i)
  filesDB <-runQuery SQL.Files.selectFolderFiles fileFolder
  let toFileUI FileDB {..} = FileUI
        { fileUI_path = fileDB_folder <> "/" <> fileDB_name
        , fileUI_name = fileDB_name
        , fileUI_size = fileDB_size
        , fileUI_ipfs = fileDB_ipfs
        , fileUI_modifiedAt = fileDB_modifiedAt
        }
  pure $ fmap toFileUI filesDB

saveFileIPFS :: ByteString -> File -> Action user ()
saveFileIPFS hash (File name contents) = do
  IPFS.CID cid <- runIPFS $ IPFS.add name contents
  runQuery SQL.Files.updateFileIPFS (hash, cid)

--------------------------------------------------------------------------------
-- Forum

createThread :: Id Unit -> NewThread -> ActionAuth (Id Thread)
createThread uid NewThread {..} = do
  user <- getUser
  let thread = Thread
        { thread_unit = uid
        , thread_author = user
        , thread_subject = newThread_subject
        }
  tid <- runQuery SQL.Forum.insertThread thread

  let post = Post
        { post_thread = tid
        , post_author = user
        , post_text = newThread_text
        }
  _ <- runQuery SQL.Forum.insertPost post

  pure tid

getThreads :: Id Unit -> ActionAuth [ThreadInfo]
getThreads uid = runQuery SQL.Forum.selectThreads uid

createPost :: Id Thread -> NewPost -> ActionAuth (Id Post)
createPost tid NewPost {..} = do
  user <- getUser
  let post = Post
        { post_thread = tid
        , post_author = user
        , post_text = newPost_text
        }
  runQuery SQL.Forum.insertPost post

getPosts :: Id Thread -> ActionAuth [PostInfo]
getPosts tid = runQuery SQL.Forum.selectPosts tid

--------------------------------------------------------------------------------
-- Mail

sendMail :: Mail.Mail -> Action user ()
sendMail = runQuery SQL.Mail.insert
