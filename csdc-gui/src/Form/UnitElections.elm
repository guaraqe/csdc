module Form.UnitElections exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import API as API

import Form
import Page
import Html exposing (Html)
import Time exposing ( millisToPosix)
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)

--------------------------------------------------------------------------------
-- Model

type alias Model =
 { description : Field String String
 , electionType : Field String String
 , visibleVotes : Field Bool Bool
 , notification : Notification
 , title : Field String String
 }


initial : Model
initial =
  { title = Field.requiredString "Title"
  , description = Field.requiredString "Description"
  , electionType = Field.requiredString "ElectionType"
  , visibleVotes = Field.requiredBool "VisibleVotes"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | title = Field.reload model.title
  , description = Field.reload model.description
  , electionType = Field.reload model.electionType
  , visibleVotes = Field.reload model.visibleVotes
  }

-- Also unit update...
parse : Model -> Maybe NewElection
parse model = Result.toMaybe <|
  Field.with model.title <| \title ->
  Field.with model.description <| \description ->
  Field.with model.electionType <| \electionType ->
  Field.with model.visibleVotes <| \visibleVotes ->
  Ok
    { title = title
    , description = description
    , choices = ["Choice 1", "Choice 2"]
    , electionType = SimpleMajority
    , visibleVotes = True
    , endingAt = Time.millisToPosix 1
    }

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { pageInfo : Page.Info
  , request : NewElection -> Cmd (API.Response a)
  , finish : a -> Cmd (Msg a)
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = config.request
  , finish = config.finish
  }

type alias Msg a = Form.Msg ModelMsg () a

type ModelMsg
  = SetTitle String
  | SetDescription String
  | SetElectionType String
  | SetVisibleVotes Bool

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetTitle val ->
      ( { model | title = Field.set val model.title }
      , Cmd.none
      )
    SetDescription val ->
      ( { model | description = Field.set val model.description }
      , Cmd.none
      )
    SetElectionType  val ->
      ( { model | electionType = Field.set val model.electionType }
      , Cmd.none
      )
   SetVisibleVotes : Bool -> (Model, Cmd Msg)
   SetVisibleVotes bool =
    ( { model | visibleVotes = bool }
    , Cmd.none
    )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html (Msg a))
view model =
  [ Input.text model.title SetTitle
  , Input.textarea model.description SetDescription
  , Input.text model.electionType SetElectionType
  , Input.text model.visibleVotes SetVisibleVotes
  , Input.button "Save" ()
  ]
