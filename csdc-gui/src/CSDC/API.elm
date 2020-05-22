module CSDC.API exposing (..)

import Http
import CSDC.Types exposing (..)
import Json.Decode as D

--------------------------------------------------------------------------------
-- Helpers

type alias Response a = Result Http.Error a

baseUrl : String
baseUrl = "http://localhost:8080/api/"

decodeNull : D.Decoder ()
decodeNull =
  D.map (\_ -> ()) (D.array D.int)

--------------------------------------------------------------------------------
-- Msg

type Msg
  = RootPerson (Response UserId)
  | UnitsPerson (Response (IdMap Member Unit))
  | SelectPerson (Id Person) (Response Person)
  | InsertPerson (Response (Id Person))
  | UpdatePerson (Response ())
  | DeletePerson (Response ())
  | RootUnit (Response (Id Unit))
  | GetUnitMembers (Response (IdMap Member (WithId Person)))
  | GetUnitChildren (Response (IdMap Subpart (WithId Unit)))
  | GetUnitParents (Response (IdMap Subpart (WithId Unit)))
  | CreateUnit (Response (WithId Member))
  | SelectUnit (Id Unit) (Response Unit)
  | InsertUnit (Response (Id Unit))
  | UpdateUnit (Response ())
  | DeleteUnit (Response ())
  | SelectMemberPerson (Response (IdMap Member Member))
  | SelectMemberUnit (Response (IdMap Member Member))
  | InsertMember (Response (Id Member))
  | DeleteMember (Response ())
  | SelectSubpartChild (Response (IdMap Subpart Subpart))
  | SelectSubpartParent (Response (IdMap Subpart Subpart))
  | InsertSubpart (Response (Id Subpart))
  | DeleteSubpart (Response ())
  | SendMessageMember (Response ())
  | SendReplyMember (Response ())
  | ViewReplyMember (Response ())
  | SendMessageSubpart (Response ())
  | SendReplySubpart (Response ())
  | ViewReplySubpart (Response ())
  | PersonInbox (Response Inbox)

--------------------------------------------------------------------------------
-- Person

rootPerson : Cmd Msg
rootPerson =
  Http.get
    { url = baseUrl ++ "person/root"
    , expect = Http.expectJson RootPerson (decodeUser decodeId)
    }

unitsPerson : Id Person -> Cmd Msg
unitsPerson id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id ++ "/units"
    , expect = Http.expectJson UnitsPerson (decodeIdMap decodeUnit)
    }

selectPerson : Id Person -> Cmd Msg
selectPerson id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id
    , expect = Http.expectJson (SelectPerson id) decodePerson
    }

insertPerson : Person -> Cmd Msg
insertPerson person =
  Http.post
    { url = baseUrl ++ "person"
    , body = Http.jsonBody (encodePerson person)
    , expect = Http.expectJson InsertPerson decodeId
    }

updatePerson : Id Person -> Person -> Cmd Msg
updatePerson id person =
  Http.post
    { url = baseUrl ++ "person/" ++ idToString id
    , body = Http.jsonBody (encodePerson person)
    , expect = Http.expectJson UpdatePerson decodeNull
    }

deletePerson : Id Person -> Cmd Msg
deletePerson = delete "person" DeletePerson

--------------------------------------------------------------------------------
-- Unit

rootUnit : Cmd Msg
rootUnit =
  Http.get
    { url = baseUrl ++ "unit/root"
    , expect = Http.expectJson RootUnit decodeId
    }

getUnitMembers : Id Unit -> Cmd Msg
getUnitMembers id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/members"
    , expect = Http.expectJson GetUnitMembers (decodeIdMap (decodeWithId decodePerson))
    }

getUnitChildren : Id Unit -> Cmd Msg
getUnitChildren id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/children"
    , expect = Http.expectJson GetUnitChildren (decodeIdMap (decodeWithId decodeUnit))
    }

getUnitParents : Id Unit -> Cmd Msg
getUnitParents id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/parents"
    , expect = Http.expectJson GetUnitParents (decodeIdMap (decodeWithId decodeUnit))
    }

createUnit : Id Person -> Cmd Msg
createUnit id =
  Http.post
    { url = baseUrl ++ "unit/create"
    , body = Http.jsonBody (encodeId id)
    , expect = Http.expectJson CreateUnit (decodeWithId decodeMember)
    }

selectUnit : Id Unit -> Cmd Msg
selectUnit id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id
    , expect = Http.expectJson (SelectUnit id) decodeUnit
    }

insertUnit : Unit -> Cmd Msg
insertUnit unit =
  Http.post
    { url = baseUrl ++ "unit"
    , body = Http.jsonBody (encodeUnit unit)
    , expect = Http.expectJson InsertUnit decodeId
    }

updateUnit : Id Unit -> Unit -> Cmd Msg
updateUnit id unit =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id
    , body = Http.jsonBody (encodeUnit unit)
    , expect = Http.expectJson UpdateUnit decodeNull
    }

deleteUnit : Id Unit -> Cmd Msg
deleteUnit = delete "unit" DeleteUnit

--------------------------------------------------------------------------------
-- Member

selectMemberPerson : Id Person -> Cmd Msg
selectMemberPerson id =
  Http.get
    { url = baseUrl ++ "member/person/" ++ idToString id
    , expect = Http.expectJson SelectMemberPerson (decodeIdMap decodeMember)
    }

selectMemberUnit : Id Unit -> Cmd Msg
selectMemberUnit id =
  Http.get
    { url = baseUrl ++ "member/unit/" ++ idToString id
    , expect = Http.expectJson SelectMemberUnit (decodeIdMap decodeMember)
    }

insertMember : Member -> Cmd Msg
insertMember member =
  Http.post
    { url = baseUrl ++ "member"
    , body = Http.jsonBody (encodeMember member)
    , expect = Http.expectJson InsertMember decodeId
    }

deleteMember : Id Member -> Cmd Msg
deleteMember = delete "member" DeleteMember

--------------------------------------------------------------------------------
-- Subpart

selectSubpartChild : Id Unit -> Cmd Msg
selectSubpartChild id =
  Http.get
    { url = baseUrl ++ "subpart/child/" ++ idToString id
    , expect = Http.expectJson SelectSubpartChild (decodeIdMap decodeSubpart)
    }

selectSubpartParent : Id Unit -> Cmd Msg
selectSubpartParent id =
  Http.get
    { url = baseUrl ++ "subpart/parent/" ++ idToString id
    , expect = Http.expectJson SelectSubpartParent (decodeIdMap decodeSubpart)
    }

insertSubpart : Subpart -> Cmd Msg
insertSubpart subpart =
  Http.post
    { url = baseUrl ++ "subpart"
    , body = Http.jsonBody (encodeSubpart subpart)
    , expect = Http.expectJson InsertSubpart decodeId
    }

deleteSubpart : Id Subpart -> Cmd Msg
deleteSubpart = delete "subpart" DeleteSubpart

--------------------------------------------------------------------------------
-- Message

sendMessageMember : Message Member -> Cmd Msg
sendMessageMember msg =
  Http.post
    { url = baseUrl ++ "message/member/send"
    , body = Http.jsonBody <| encodeMessage encodeMember msg
    , expect = Http.expectJson SendMessageMember decodeNull
    }

sendReplyMember : Reply Member -> Cmd Msg
sendReplyMember reply =
  Http.post
    { url = baseUrl ++ "message/member/reply"
    , body = Http.jsonBody <| encodeReply reply
    , expect = Http.expectJson SendReplyMember decodeNull
    }

viewReplyMember : Id (Reply Member) -> Cmd Msg
viewReplyMember id =
  Http.post
    { url = baseUrl ++ "message/member/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson SendReplyMember decodeNull
    }

sendMessageSubpart : Message Subpart -> Cmd Msg
sendMessageSubpart msg =
  Http.post
    { url = baseUrl ++ "message/subpart/send"
    , body = Http.jsonBody <| encodeMessage encodeSubpart msg
    , expect = Http.expectJson SendMessageSubpart decodeNull
    }

sendReplySubpart : Reply Subpart -> Cmd Msg
sendReplySubpart reply =
  Http.post
    { url = baseUrl ++ "message/subpart/reply"
    , body = Http.jsonBody <| encodeReply reply
    , expect = Http.expectJson SendReplySubpart decodeNull
    }

viewReplySubpart : Id (Reply Subpart) -> Cmd Msg
viewReplySubpart id =
  Http.post
    { url = baseUrl ++ "message/subpart/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson SendReplySubpart decodeNull
    }

personInbox : Id Person -> Cmd Msg
personInbox id =
  Http.get
    { url = baseUrl ++ "message/inbox/" ++ idToString id
    , expect = Http.expectJson PersonInbox decodeInbox
    }

--------------------------------------------------------------------------------
-- Delete

delete : String -> (Result Http.Error () -> Msg) -> Id a -> Cmd Msg
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
