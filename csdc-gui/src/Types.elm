module Types exposing (..)

import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encoder exposing (Value)
import String
import Time exposing (Posix, Month (..))
import UUID exposing (UUID)
import Dict exposing (Dict)

--------------------------------------------------------------------------------
-- Login

type alias NewUser =
  { name : String
  , email : String
  , password : String
  }

encodeNewUser : NewUser -> Value
encodeNewUser newUser =
  Encoder.object
    [ ("name", Encoder.string newUser.name)
    , ("email", Encoder.string newUser.email)
    , ("password", Encoder.string newUser.password)
    ]

type alias Login =
  { email : String
  , password : String
  }

encodeLogin : Login -> Value
encodeLogin login =
  Encoder.object
    [ ("email", Encoder.string login.email)
    , ("password", Encoder.string login.password)
    ]

--------------------------------------------------------------------------------
-- Id

type Id a = Id UUID

encodeId : Id a -> Value
encodeId (Id n) = Encoder.string (UUID.toString n)

decodeId : Decoder (Id a)
decodeId = Decoder.map Id UUID.jsonDecoder

idToString : Id a -> String
idToString (Id n) = UUID.toString n

idFromString : String -> Maybe (Id a)
idFromString s =
  case UUID.fromString s of
    Err _ -> Nothing
    Ok a -> Just (Id a)

type alias WithId a =
  { id : Id a
  , value : a
  }

decodeWithId : Decoder a -> Decoder (WithId a)
decodeWithId decode =
  Decoder.map2 WithId
    (Decoder.field "id" decodeId)
    (Decoder.field "value" decode)

lookup : (a -> Bool) -> List a -> Maybe a
lookup p = List.head << List.filter p

lookupById : Id a -> List { obj | id : Id a } -> Maybe { obj | id : Id a }
lookupById id = lookup (\obj -> obj.id == id)

--------------------------------------------------------------------------------
-- Time

decodePosix : Decoder Posix
decodePosix =
  Decoder.map
    (Time.millisToPosix << floor)
    Decoder.float

viewPosixAt : Time.Zone -> Posix -> String
viewPosixAt zone posix =
  let
    y = String.fromInt <| Time.toYear zone posix
    m = viewMonth <| Time.toMonth zone posix
    d = String.fromInt <| Time.toDay zone posix
    h = padNumber <| String.fromInt <| Time.toHour zone posix
    mi = padNumber <| String.fromInt <| Time.toMinute zone posix

    padNumber s = if String.length s == 1 then "0" ++ s else s
  in
    String.join "/" [y,m,d] ++ " " ++ String.join ":" [h,mi]

viewMonth : Month -> String
viewMonth month =
  case month of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

--------------------------------------------------------------------------------
-- User

type alias UserId = Id Person

--------------------------------------------------------------------------------
-- FilePath

type FilePath = FilePath String

filePath : FilePath -> String
filePath (FilePath path) = "files/" ++ path

decodeFilePath : Decoder FilePath
decodeFilePath = Decoder.map FilePath Decoder.string

--------------------------------------------------------------------------------
-- Person

type alias Person =
  { name : String
  , email : String
  , description : String
  , image : FilePath
  , createdAt : Posix
  }

decodePerson : Decoder Person
decodePerson =
  Decoder.map5 Person
    (Decoder.field "name" Decoder.string)
    (Decoder.field "email" Decoder.string)
    (Decoder.field "description" Decoder.string)
    (Decoder.field "image" decodeFilePath)
    (Decoder.field "createdAt" decodePosix)

type alias PersonUpdate =
  { name : String
  , description : String
  }

encodePersonUpdate : PersonUpdate -> Value
encodePersonUpdate person =
  Encoder.object
    [ ("name", Encoder.string person.name)
    , ("description", Encoder.string person.description)
    ]

type alias PersonMember =
  { memberId : Id Member
  , unitId : Id Unit
  , unit : Unit
  }

decodePersonMember : Decoder PersonMember
decodePersonMember =
  Decoder.map3 PersonMember
    (Decoder.field "memberId" decodeId)
    (Decoder.field "unitId" decodeId)
    (Decoder.field "unit" decodeUnit)

type alias PersonInfo =
  { id : Id Person
  , person : Person
  , members : List PersonMember
  , unitsForMessage : List (WithId Unit)
  }

decodePersonInfo : Decoder PersonInfo
decodePersonInfo =
  Decoder.map4 PersonInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "person" decodePerson)
    (Decoder.field "members" (Decoder.list decodePersonMember))
    (Decoder.field "unitsForMessage" (Decoder.list (decodeWithId decodeUnit)))

--------------------------------------------------------------------------------
-- Unit

type alias Unit =
  { name : String
  , description : String
  , chairId : Id Person
  , image : FilePath
  , createdAt : Posix
  }

decodeUnit : Decoder Unit
decodeUnit =
  Decoder.map5 Unit
    (Decoder.field "name" Decoder.string)
    (Decoder.field "description" Decoder.string)
    (Decoder.field "chairId" decodeId)
    (Decoder.field "image" decodeFilePath)
    (Decoder.field "createdAt" decodePosix)

type alias NewUnit =
  { name : String
  , description : String
  }

encodeNewUnit : NewUnit -> Value
encodeNewUnit unit =
  Encoder.object
    [ ("name", Encoder.string unit.name)
    , ("description", Encoder.string unit.description)
    ]

type alias UnitUpdate =
  { name : String
  , description : String
  }

encodeUnitUpdate : UnitUpdate -> Value
encodeUnitUpdate unit =
  Encoder.object
    [ ("name", Encoder.string unit.name)
    , ("description", Encoder.string unit.description)
    ]

type alias UnitMember =
  { memberId : Id Member
  , personId : Id Person
  , person : Person
  }

decodeUnitMember : Decoder UnitMember
decodeUnitMember =
  Decoder.map3 UnitMember
    (Decoder.field "memberId" decodeId)
    (Decoder.field "personId" decodeId)
    (Decoder.field "person" decodePerson)

type alias UnitSubpart =
  { subpartId : Id Subpart
  , level : Int
  , unitId : Id Unit
  , unit : Unit
  }

decodeUnitSubpart : Decoder UnitSubpart
decodeUnitSubpart =
  Decoder.map4 UnitSubpart
    (Decoder.field "subpartId" decodeId)
    (Decoder.field "level" Decoder.int)
    (Decoder.field "unitId" decodeId)
    (Decoder.field "unit" decodeUnit)

type alias UnitInfo =
  { id : Id Unit
  , unit : Unit
  , members : List UnitMember
  , children : List UnitSubpart
  , parents : List UnitSubpart
  , userId : Id Person
  , isMember : Bool
  , isIndirectMember : Bool
  , isAdmin : Bool
  , isMembershipPending : Bool
  , electionsPending : Int
  , unitsForMessage : List (WithId Unit)
  }

decodeUnitInfo : Decoder UnitInfo
decodeUnitInfo =
  Decoder.succeed UnitInfo
    |> andMap (Decoder.field "id" decodeId)
    |> andMap (Decoder.field "unit" decodeUnit)
    |> andMap (Decoder.field "members" (Decoder.list decodeUnitMember ))
    |> andMap (Decoder.field "children" (Decoder.list decodeUnitSubpart))
    |> andMap (Decoder.field "parents" (Decoder.list decodeUnitSubpart))
    |> andMap (Decoder.field "userId" decodeId)
    |> andMap (Decoder.field "isMember" Decoder.bool)
    |> andMap (Decoder.field "isIndirectMember" Decoder.bool)
    |> andMap (Decoder.field "isAdmin" Decoder.bool)
    |> andMap (Decoder.field "isMembershipPending" Decoder.bool)
    |> andMap (Decoder.field "electionsPending" Decoder.int)
    |> andMap (Decoder.field "unitsForMessage" (Decoder.list (decodeWithId decodeUnit)))

andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap = Decoder.map2 (\a f -> f a)

--------------------------------------------------------------------------------
-- Member

type alias Member =
  { personId : Id Person
  , unitId : Id Unit
  , createdAt : Posix
  }

decodeMember : Decoder Member
decodeMember =
  Decoder.map3 Member
    (Decoder.field "personId" decodeId)
    (Decoder.field "unitId" decodeId)
    (Decoder.field "createdAt" decodePosix)

type alias NewMember =
  { personId : Id Person
  , unitId: Id Unit
  }

encodeNewMember : NewMember -> Value
encodeNewMember member =
  Encoder.object
    [ ("personId", encodeId member.personId)
    , ("unitId", encodeId member.unitId)
    ]

decodeNewMember : Decoder NewMember
decodeNewMember =
  Decoder.map2 NewMember
    (Decoder.field "personId" decodeId)
    (Decoder.field "unitId" decodeId)

--------------------------------------------------------------------------------
-- Subpart

type alias Subpart =
  { childId: Id Unit
  , parentId: Id Unit
  , createdAt : Posix
  }

decodeSubpart : Decoder Subpart
decodeSubpart =
  Decoder.map3 Subpart
    (Decoder.field "childId" decodeId)
    (Decoder.field "parentId" decodeId)
    (Decoder.field "createdAt" decodePosix)

type alias NewSubpart =
  { childId: Id Unit
  , parentId: Id Unit
  }

decodeNewSubpart : Decoder NewSubpart
decodeNewSubpart =
  Decoder.map2 NewSubpart
    (Decoder.field "childId" decodeId)
    (Decoder.field "parentId" decodeId)

encodeNewSubpart : NewSubpart -> Value
encodeNewSubpart subpart =
  Encoder.object
    [ ("childId", encodeId subpart.childId)
    , ("parentId", encodeId subpart.parentId)
    ]

--------------------------------------------------------------------------------
-- Message

type MessageStatus = Waiting | Accepted | Rejected

decodeMessageStatus : Decoder MessageStatus
decodeMessageStatus =
  decodeString <| \s ->
    case s of
      "Waiting" ->
        Decoder.succeed Waiting
      "Accepted" ->
        Decoder.succeed Accepted
      "Rejected" ->
        Decoder.succeed Rejected
      _ ->
        Decoder.fail <| "Invalid MessageStatus: " ++ s

encodeMessageStatus : MessageStatus -> Value
encodeMessageStatus s =
  case s of
    Waiting -> Encoder.string "Waiting"
    Accepted -> Encoder.string "Accepted"
    Rejected -> Encoder.string "Rejected"

type MessageType = Invitation | Submission

decodeMessageType : Decoder MessageType
decodeMessageType =
  decodeString <| \s ->
    case s of
      "Invitation" ->
        Decoder.succeed Invitation
      "Submission" ->
        Decoder.succeed Submission
      _ ->
        Decoder.fail <| "Invalid MessageType: " ++ s

encodeMessageType : MessageType -> Value
encodeMessageType s =
  case s of
    Invitation -> Encoder.string "Invitation"
    Submission -> Encoder.string "Submission"

type alias Message a =
  { messageType : MessageType
  , text : String
  , status : MessageStatus
  , value : a
  }

decodeMessage : Decoder a -> Decoder (Message a)
decodeMessage decode =
  Decoder.map4 Message
    (Decoder.field "messageType" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeMessageStatus)
    (Decoder.field "value" decode)

type alias NewMessage a =
  { messageType : MessageType
  , text : String
  , value : a
  }

encodeNewMessage : (a -> Value) -> NewMessage a -> Value
encodeNewMessage encode m =
  Encoder.object
    [ ("messageType", encodeMessageType m.messageType)
    , ("text", Encoder.string m.text)
    , ("value", encode m.value)
    ]

type alias MessageInfo a =
  { id : Id (Message a)
  , messageType : MessageType
  , text : String
  , status : MessageStatus
  , value : a
  , left : String
  , right : String
  }

decodeMessageInfo : Decoder a -> Decoder (MessageInfo a)
decodeMessageInfo decode =
  Decoder.map7 MessageInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "messageType" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeMessageStatus)
    (Decoder.field "value" decode)
    (Decoder.field "left" Decoder.string)
    (Decoder.field "right" Decoder.string)

--------------------------------------------------------------------------------
-- Reply

type ReplyStatus = Seen | NotSeen

decodeReplyStatus : Decoder ReplyStatus
decodeReplyStatus =
  decodeString <| \s ->
    case s of
      "Seen" ->
        Decoder.succeed Seen
      "NotSeen" ->
        Decoder.succeed NotSeen
      _ ->
        Decoder.fail <| "Invalid ReplyStatus: " ++ s

encodeReplyStatus : ReplyStatus -> Value
encodeReplyStatus s =
  case s of
    Seen -> Encoder.string "Seen"
    NotSeen -> Encoder.string "NotSeen"

type ReplyType = Accept | Reject

decodeReplyType : Decoder ReplyType
decodeReplyType =
  decodeString <| \s ->
    case s of
      "Accept" ->
        Decoder.succeed Accept
      "Reject" ->
        Decoder.succeed Reject
      _ ->
        Decoder.fail <| "Invalid ReplyType: " ++ s

encodeReplyType : ReplyType -> Value
encodeReplyType s =
  case s of
    Accept -> Encoder.string "Accept"
    Reject -> Encoder.string "Reject"

type alias Reply a =
  { replyType : ReplyType
  , messageType : MessageType
  , text : String
  , status : ReplyStatus
  , id : Id (Message a)
  }

decodeReply : Decoder (Reply a)
decodeReply =
  Decoder.map5 Reply
    (Decoder.field "replyType" decodeReplyType)
    (Decoder.field "messageType" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeReplyStatus)
    (Decoder.field "id" decodeId)

type alias NewReply a =
  { replyType : ReplyType
  , text : String
  , messageId : Id (Message a)
  }

encodeNewReply : NewReply a -> Value
encodeNewReply m =
  Encoder.object
    [ ("replyType", encodeReplyType m.replyType)
    , ("text", Encoder.string m.text)
    , ("messageId", encodeId m.messageId)
    ]

type alias ReplyInfo a =
  { id : Id (Reply a)
  , replyType : ReplyType
  , messageType : MessageType
  , text : String
  , status : ReplyStatus
  , message : MessageInfo a
  }

decodeReplyInfo : Decoder a -> Decoder (ReplyInfo a)
decodeReplyInfo decode =
  Decoder.map6 ReplyInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "replyType" decodeReplyType)
    (Decoder.field "messageType" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeReplyStatus)
    (Decoder.field "message" (decodeMessageInfo decode))

--------------------------------------------------------------------------------
-- Inbox

type alias Inbox =
  { messageMember : List (MessageInfo NewMember)
  , replyMember : List (ReplyInfo NewMember)
  , messageSubpart : List (MessageInfo NewSubpart)
  , replySubpart : List (ReplyInfo NewSubpart)
  }

emptyInbox : Inbox
emptyInbox =
  { messageMember = []
  , replyMember = []
  , messageSubpart = []
  , replySubpart = []
  }

decodeInbox : Decoder Inbox
decodeInbox =
  Decoder.map4 Inbox
    (Decoder.field "messageMember" (Decoder.list <| decodeMessageInfo decodeNewMember))
    (Decoder.field "replyMember" (Decoder.list <| decodeReplyInfo decodeNewMember))
    (Decoder.field "messageSubpart" (Decoder.list <| decodeMessageInfo decodeNewSubpart))
    (Decoder.field "replySubpart" (Decoder.list <| decodeReplyInfo decodeNewSubpart))

--------------------------------------------------------------------------------
-- Search

type SearchId = SearchUnit (Id Unit) | SearchPerson (Id Person)

decodeSearchId : Decoder SearchId
decodeSearchId =
  Decoder.field "tag" Decoder.string
    |> Decoder.andThen (\tag ->
      if tag == "SearchUnit"
      then Decoder.map SearchUnit <| Decoder.field "contents" decodeId
      else if tag == "SearchPerson"
      then Decoder.map SearchPerson <| Decoder.field "contents" decodeId
      else Decoder.fail <| "SearchId: Unknown tag " ++ tag
    )

type alias SearchResult a =
  { name : String
  , id : a
  }

decodeSearchResult : Decoder a -> Decoder (SearchResult a)
decodeSearchResult decode =
  Decoder.map2 SearchResult
    (Decoder.field "name" Decoder.string)
    (Decoder.field "id" decode)

--------------------------------------------------------------------------------
-- File

type alias Base64File =
  { name : String
  , contents : String
  }

encodeBase64File : Base64File -> Value
encodeBase64File file =
  Encoder.object
    [ ("name", Encoder.string file.name)
    , ("contents", Encoder.string file.contents)
    ]

type alias FileUI =
  { path : FilePath
  , name : String
  , size : Int
  , modifiedAt : Posix
  }

decodeFileUI : Decoder FileUI
decodeFileUI =
  Decoder.map4 FileUI
    (Decoder.field "path" decodeFilePath)
    (Decoder.field "name" Decoder.string)
    (Decoder.field "size" Decoder.int)
    (Decoder.field "modifiedAt" decodePosix)

--------------------------------------------------------------------------------
-- Forum

type alias NewThread =
  { subject : String
  , text : String
  }

encodeNewThread : NewThread -> Value
encodeNewThread newThread =
  Encoder.object
    [ ("subject", Encoder.string newThread.subject)
    , ("text", Encoder.string newThread.text)
    ]

type alias Thread =
  { unitId : Id Unit
  , authorId : Id Person
  , subject : String
  }

type alias ThreadInfo =
  { id : Id Thread
  , unitId : Id Unit
  , authorId : Id Person
  , authorName : String
  , subject : String
  , createdAt : Posix
  , last : Posix
  , messages : Int
  }

decodeThreadInfo : Decoder ThreadInfo
decodeThreadInfo =
  Decoder.map8 ThreadInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "unitId" decodeId)
    (Decoder.field "authorId" decodeId)
    (Decoder.field "authorName" Decoder.string)
    (Decoder.field "subject" Decoder.string)
    (Decoder.field "createdAt" decodePosix)
    (Decoder.field "last" decodePosix)
    (Decoder.field "messages" Decoder.int)

type alias NewPost =
  { text : String
  }

encodeNewPost : NewPost -> Value
encodeNewPost newPost =
  Encoder.object
    [ ("text", Encoder.string newPost.text)
    ]

type alias Post =
  { threadId : Id Thread
  , authorId : Id Person
  , text : String
  }

type alias PostInfo =
  { id : Id Post
  , authorId : Id Person
  , authorName : String
  , authorImage : FilePath
  , text : String
  , createdAt : Posix
  }

decodePostInfo : Decoder PostInfo
decodePostInfo =
  Decoder.map6 PostInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "authorId" decodeId)
    (Decoder.field "authorName" Decoder.string)
    (Decoder.field "authorImage" decodeFilePath)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "createdAt" decodePosix)

--------------------------------------------------------------------------------
-- Mail Invitation

type alias MailInvitation =
  { message : String
  , invitees : List String
  }

encodeMailInvitation : MailInvitation -> Value
encodeMailInvitation mailInvitation =
  Encoder.object
    [ ("message", Encoder.string mailInvitation.message)
    , ("invitees", Encoder.list Encoder.string mailInvitation.invitees)
    ]

--------------------------------------------------------------------------------
-- Election

type ElectionType = MajorityConsensus | SimpleMajority

decodeElectionType : Decoder ElectionType
decodeElectionType =
  decodeString <| \s ->
    case s of
      "MajorityConsensus" ->
        Decoder.succeed MajorityConsensus
      "SimpleMajority" ->
        Decoder.succeed SimpleMajority
      _ ->
        Decoder.fail <| "Invalid ElectionType: " ++ s

encodeElectionType : ElectionType -> Value
encodeElectionType s =
  case s of
    MajorityConsensus -> Encoder.string "MajorityConsensus"
    SimpleMajority -> Encoder.string "SimpleMajority"

type alias ElectionChoice = String

decodeElectionChoice : Decoder ElectionChoice
decodeElectionChoice = Decoder.string

encodeElectionChoice : ElectionChoice -> Value
encodeElectionChoice = Encoder.string

type alias Election =
  { unitId : Id Unit
  , title : String
  , description : String
  , choices : List ElectionChoice
  , electionType : ElectionType
  , visibleVotes : Bool
  , endingAt : Posix
  , result : Maybe ElectionChoice
  , resultComputedAt : Maybe Posix
  }

decodeElection : Decoder Election
decodeElection =
  Decoder.succeed Election
    |> andMap (Decoder.field "unitId" decodeId)
    |> andMap (Decoder.field "title" Decoder.string)
    |> andMap (Decoder.field "description" Decoder.string)
    |> andMap (Decoder.field "choices" (Decoder.list decodeElectionChoice))
    |> andMap (Decoder.field "electionType" decodeElectionType)
    |> andMap (Decoder.field "visibleVotes" Decoder.bool)
    |> andMap (Decoder.field "endingAt"  decodePosix)
    |> andMap (Decoder.field "result" (Decoder.nullable decodeElectionChoice))
    |> andMap (Decoder.field "resultComputedAt" (Decoder.nullable decodePosix))

type alias ElectionInfo =
  { electionId : Id Election
  , election : Election
  , votedAt : Maybe Posix
  , totalVotes : Int
  }

decodeElectionInfo : Decoder ElectionInfo
decodeElectionInfo =
  Decoder.succeed ElectionInfo
    |> andMap (Decoder.field "electionId" decodeId)
    |> andMap (Decoder.field "election" decodeElection)
    |> andMap (Decoder.field "votedAt" (Decoder.nullable decodePosix))
    |> andMap (Decoder.field "totalVotes" Decoder.int)

type alias NewElection =
    { title : String
    , description : String
    , choices : List ElectionChoice
    , electionType : ElectionType
    , visibleVotes : Bool
    , endingAt : Posix
    }

encodeNewElection : NewElection -> Value
encodeNewElection newElection =
  Encoder.object
    [ ("title", Encoder.string newElection.title)
    , ("description", Encoder.string newElection.description )
    , ( "choices", Encoder.list encodeElectionChoice newElection.choices )
    , ( "electionType", encodeElectionType  newElection.electionType )
    , ( "visibleVotes", Encoder.bool newElection.visibleVotes )
    , ( "endingAt", Encoder.int <| Time.posixToMillis newElection.endingAt )
    ]

--------------------------------------------------------------------------------
-- Votes

type VotePayload
  = VotePayloadMajorityConsensus (Dict ElectionChoice Grade)
  | VotePayloadSimpleMajority (Maybe ElectionChoice)

encodeVotePayload : VotePayload -> Value
encodeVotePayload p =
  case p of
    VotePayloadMajorityConsensus dict ->
      Encoder.object
        [ ( "tag", Encoder.string "VotePayloadMajorityConsensus" )
        , ( "contents", Encoder.dict identity encodeGrade dict )
        ]

    VotePayloadSimpleMajority possibleChoice ->
      Encoder.object
        [ ( "tag", Encoder.string "VotePayloadSimpleMajority")
        , ( "contents",
            case possibleChoice of
              Nothing -> Encoder.null
              Just choice -> Encoder.string choice
          )
        ]

-- This is needed for Id references
type alias Vote =
  { electionId : Id Election
  , payload : VotePayload
  }

type Grade
  = GradeExcellent
  | GradeVeryGood
  | GradeGood
  | GradeAcceptable
  | GradeBad
  | GradeVeryBad

encodeGrade : Grade -> Value
encodeGrade grade =
    case grade of
        GradeExcellent ->
            Encoder.string "GradeExcellent"
        GradeVeryGood ->
            Encoder.string "GradeVeryGood"
        GradeGood ->
            Encoder.string "GradeGood"
        GradeAcceptable ->
            Encoder.string "GradeAcceptable"
        GradeBad ->
            Encoder.string "GradeBad"
        GradeVeryBad ->
            Encoder.string "GradeVeryBad"

--------------------------------------------------------------------------------
-- Helpers

decodeString : (String -> Decoder a) -> Decoder a
decodeString f = Decoder.andThen f Decoder.string
