module API exposing (..)

import Types exposing (..)

import File exposing (File)
import Http
import Json.Decode as D
import Url.Builder

--------------------------------------------------------------------------------
-- Helpers

type alias Response a = Result Http.Error a

baseUrl : String
baseUrl = "/api/"

decodeNull : D.Decoder ()
decodeNull =
  D.map (\_ -> ()) (D.array D.int)

ignoreResult : Result e a -> Result e ()
ignoreResult = Result.map (\_ -> ())

url : List String -> String
url l = Url.Builder.absolute ("api" :: l) []

--------------------------------------------------------------------------------
-- Auth

signin : Login -> Cmd (Response ())
signin login =
  Http.post
    { url = "/signin"
    , body = Http.jsonBody (encodeLogin login)
    , expect = Http.expectWhatever identity
    }

signup : NewUser -> Cmd (Response (Id Person))
signup newUser =
  Http.post
    { url = "/signup"
    , body = Http.jsonBody (encodeNewUser newUser)
    , expect = Http.expectJson identity decodeId
    }

signout : Cmd (Response ())
signout =
  Http.get
    { url = "/signout"
    , expect = Http.expectJson identity decodeNull
    }

--------------------------------------------------------------------------------
-- User

getUserInfo : Cmd (Response PersonInfo)
getUserInfo =
  Http.get
    { url = baseUrl ++ "user/info"
    , expect = Http.expectJson identity decodePersonInfo
    }

getUserInbox : Cmd (Response Inbox)
getUserInbox =
  Http.get
    { url = baseUrl ++ "user/inbox"
    , expect = Http.expectJson identity decodeInbox
    }

getUserUnits : Cmd (Response (List (WithId Unit)))
getUserUnits =
  Http.get
    { url = baseUrl ++ "user/units"
    , expect = Http.expectJson identity (D.list (decodeWithId decodeUnit))
    }

searchUnits : String -> Cmd (Response (List (WithId Unit)))
searchUnits input =
  Http.get
    { url = url ["search", "units", input]
    , expect = Http.expectJson identity (D.list (decodeWithId decodeUnit))
    }

searchAll : String -> Cmd (Response (List (SearchResult SearchId)))
searchAll input =
  Http.get
    { url = url ["search", "all", input]
    , expect = Http.expectJson identity (D.list (decodeSearchResult decodeSearchId))
    }

--------------------------------------------------------------------------------
-- Person

getPersonInfo : Id Person -> Cmd (Response PersonInfo)
getPersonInfo id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id ++ "/info"
    , expect = Http.expectJson identity decodePersonInfo
    }

updatePerson : Id Person -> PersonUpdate -> Cmd (Response ())
updatePerson id person =
  Http.post
    { url = baseUrl ++ "person/" ++ idToString id
    , body = Http.jsonBody (encodePersonUpdate person)
    , expect = Http.expectJson identity decodeNull
    }

updatePersonImage : Id Person -> Base64File -> Cmd (Response ())
updatePersonImage id file =
  Http.post
    { url = baseUrl ++ "person/" ++ idToString id ++ "/image"
    , body = Http.jsonBody (encodeBase64File file)
    , expect = Http.expectJson identity decodeNull
    }

--------------------------------------------------------------------------------
-- Unit

getUnitInfo : Id Unit -> Cmd (Response UnitInfo)
getUnitInfo id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/info"
    , expect = Http.expectJson identity decodeUnitInfo
    }

getUnitChildren : Id Unit -> Cmd (Response (List UnitSubpart))
getUnitChildren id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/children"
    , expect = Http.expectJson identity (D.list decodeUnitSubpart)
    }

getUnitParents : Id Unit -> Cmd (Response (List UnitSubpart))
getUnitParents id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/parents"
    , expect = Http.expectJson identity (D.list decodeUnitSubpart)
    }

createUnit : NewUnit -> Cmd (Response (Id Unit))
createUnit unit =
  Http.post
    { url = baseUrl ++ "unit"
    , body = Http.jsonBody (encodeNewUnit unit)
    , expect = Http.expectJson identity decodeId
    }

selectUnit : Id Unit -> Cmd (Response Unit)
selectUnit id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id
    , expect = Http.expectJson identity decodeUnit
    }

updateUnit : Id Unit -> UnitUpdate -> Cmd (Response ())
updateUnit id unit =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id
    , body = Http.jsonBody (encodeUnitUpdate unit)
    , expect = Http.expectJson identity decodeNull
    }

updateUnitImage : Id Unit -> Base64File -> Cmd (Response ())
updateUnitImage id file =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/image"
    , body = Http.jsonBody (encodeBase64File file)
    , expect = Http.expectJson identity decodeNull
    }

updateUnitChair : Id Unit -> Id Person -> Cmd (Response ())
updateUnitChair uid pid =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString uid ++ "/chair"
    , body = Http.jsonBody (encodeId pid)
    , expect = Http.expectJson identity decodeNull
    }

deleteUnit : Id Unit -> Cmd (Response ())
deleteUnit = delete "unit" identity

insertUnitFile : Id Unit -> File -> Cmd (Response ())
insertUnitFile id file =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/files"
    , body = Http.multipartBody
        [ Http.filePart "file" file
        ]
    , expect = Http.expectJson identity decodeNull
    }

getUnitFiles : Id Unit -> Cmd (Response (List FileUI))
getUnitFiles id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/files"
    , expect = Http.expectJson identity (D.list decodeFileUI)
    }

sendMailInvitation : Id Unit -> MailInvitation -> Cmd (Response ())
sendMailInvitation id mailInvitation =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/invitation"
    , body = Http.jsonBody (encodeMailInvitation mailInvitation)
    , expect = Http.expectJson identity decodeNull
    }

--------------------------------------------------------------------------------
-- Member

insertMember : NewMember -> Cmd (Response (Id Member))
insertMember member =
  Http.post
    { url = baseUrl ++ "member"
    , body = Http.jsonBody (encodeNewMember member)
    , expect = Http.expectJson identity decodeId
    }

deleteMember : Id Member -> Cmd (Response ())
deleteMember = delete "member" identity

--------------------------------------------------------------------------------
-- Subpart

insertSubpart : NewSubpart -> Cmd (Response (Id Subpart))
insertSubpart subpart =
  Http.post
    { url = baseUrl ++ "subpart"
    , body = Http.jsonBody (encodeNewSubpart subpart)
    , expect = Http.expectJson identity decodeId
    }

deleteSubpart : Id Subpart -> Cmd (Response ())
deleteSubpart = delete "subpart" identity

--------------------------------------------------------------------------------
-- Message

sendMessageMember : NewMessage NewMember -> Cmd (Response (Id (Message NewMember)))
sendMessageMember msg =
  Http.post
    { url = baseUrl ++ "message/member/send"
    , body = Http.jsonBody <| encodeNewMessage encodeNewMember msg
    , expect = Http.expectJson identity decodeId
    }

sendReplyMember : NewReply NewMember -> Cmd (Response ())
sendReplyMember reply =
  Http.post
    { url = baseUrl ++ "message/member/reply"
    , body = Http.jsonBody <| encodeNewReply reply
    , expect = Http.expectJson ignoreResult decodeId
    }

viewReplyMember : Id (Reply NewMember) -> Cmd (Response ())
viewReplyMember id =
  Http.post
    { url = baseUrl ++ "message/member/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson identity decodeNull
    }

sendMessageSubpart : NewMessage NewSubpart -> Cmd (Response (Id (Message NewSubpart)))
sendMessageSubpart msg =
  Http.post
    { url = baseUrl ++ "message/subpart/send"
    , body = Http.jsonBody <| encodeNewMessage encodeNewSubpart msg
    , expect = Http.expectJson identity decodeId
    }

sendReplySubpart : NewReply NewSubpart -> Cmd (Response ())
sendReplySubpart reply =
  Http.post
    { url = baseUrl ++ "message/subpart/reply"
    , body = Http.jsonBody <| encodeNewReply reply
    , expect = Http.expectJson ignoreResult decodeId
    }

viewReplySubpart : Id (Reply NewSubpart) -> Cmd (Response ())
viewReplySubpart id =
  Http.post
    { url = baseUrl ++ "message/subpart/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson identity decodeNull
    }

unitInbox : Id Unit -> Cmd (Response Inbox)
unitInbox id =
  Http.get
    { url = baseUrl ++ "message/inbox/unit/" ++ idToString id
    , expect = Http.expectJson identity decodeInbox
    }

--------------------------------------------------------------------------------
-- Delete

delete : String -> (Result Http.Error () -> b) -> Id a -> Cmd b
delete name msg id =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = baseUrl ++ name ++ "/" ++ idToString id
    , body = Http.emptyBody
    , expect = Http.expectJson msg decodeNull
    , timeout = Nothing
    , tracker = Nothing
    }

--------------------------------------------------------------------------------
-- Forum

createUnitThread : Id Unit -> NewThread -> Cmd (Response (Id Thread))
createUnitThread uid newThread =
  Http.post
    { url = baseUrl ++ "forum/unit/" ++ idToString uid
    , body = Http.jsonBody <| encodeNewThread newThread
    , expect = Http.expectJson identity decodeId
    }

getUnitThreads : Id Unit -> Cmd (Response (List ThreadInfo))
getUnitThreads uid =
  Http.get
    { url = baseUrl ++ "forum/unit/" ++ idToString uid
    , expect = Http.expectJson identity (D.list decodeThreadInfo)
    }

createThreadPost : Id Thread -> NewPost -> Cmd (Response (Id Post))
createThreadPost uid newPost =
  Http.post
    { url = baseUrl ++ "forum/thread/" ++ idToString uid
    , body = Http.jsonBody <| encodeNewPost newPost
    , expect = Http.expectJson identity decodeId
    }

getThreadPosts : Id Thread -> Cmd (Response (List PostInfo))
getThreadPosts uid =
  Http.get
    { url = baseUrl ++ "forum/thread/" ++ idToString uid
    , expect = Http.expectJson identity (D.list decodePostInfo)
    }

--------------------------------------------------------------------------------
-- Election

  -- POST election/unit/<unit-uuid>/
createElection : Id Unit -> NewElection -> Cmd (Response (Id Election))
createElection uid newElection =
  Http.post
    { url = baseUrl ++ "election/unit/" ++ idToString uid
    , body = Http.jsonBody <| encodeNewElection newElection
    , expect = Http.expectJson identity decodeId
    }

    -- GET election/unit/<unit-uuid>/
getElections : Id Unit -> Cmd (Response (List ElectionInfo))
getElections uid =
  Http.get
    { url = baseUrl ++ "election/unit/" ++ idToString uid
    , expect = Http.expectJson identity (D.list decodeElectionInfo)
    }

-- DELETE election/<election-id>
deleteElection : Id Election -> Cmd (Response ())
deleteElection = delete "election" identity

 -- POST election/<election-id>/vote
addVote : Id Election -> VotePayload -> Cmd (Response (Id Vote))
addVote  uid votePayload =
  Http.post
    { url = baseUrl ++ "election/" ++ idToString uid ++ "vote/"
    , body = Http.jsonBody <| encodeVotePayload votePayload
    , expect = Http.expectJson identity decodeId
    }
