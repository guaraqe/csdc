module CSDC.API exposing (..)

import Http
import CSDC.Types exposing (..)
import Json.Decode as D

--------------------------------------------------------------------------------
-- Helpers

baseUrl : String
baseUrl = "http://localhost:8080/api/"

decodeNull : D.Decoder ()
decodeNull =
  D.map (\_ -> ()) (D.array D.int)

--------------------------------------------------------------------------------
-- Msg

type Msg
  = RootPerson (Result Http.Error UserId)
  | UnitsPerson (Result Http.Error (IdMap Member Unit))
  | SelectPerson (Result Http.Error Person)
  | InsertPerson (Result Http.Error (Id Person))
  | UpdatePerson (Result Http.Error ())
  | DeletePerson (Result Http.Error ())
  | RootUnit (Result Http.Error (Id Unit))
  | GetUnitMembers (Result Http.Error (IdMap Member (WithId Person)))
  | GetUnitChildren (Result Http.Error (IdMap Subpart (WithId Unit)))
  | GetUnitParents (Result Http.Error (IdMap Subpart (WithId Unit)))
  | CreateUnit (Result Http.Error (WithId Member))
  | SelectUnit (Id Unit) (Result Http.Error Unit)
  | InsertUnit (Result Http.Error (Id Unit))
  | UpdateUnit (Result Http.Error ())
  | DeleteUnit (Result Http.Error ())
  | SelectMemberPerson (Result Http.Error (IdMap Member Member))
  | SelectMemberUnit (Result Http.Error (IdMap Member Member))
  | InsertMember (Result Http.Error (Id Member))
  | DeleteMember (Result Http.Error ())
  | SelectSubpartChild (Result Http.Error (IdMap Subpart Subpart))
  | SelectSubpartParent (Result Http.Error (IdMap Subpart Subpart))
  | InsertSubpart (Result Http.Error (Id Subpart))
  | DeleteSubpart (Result Http.Error ())

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
    , expect = Http.expectJson SelectPerson decodePerson
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
