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
import Html.Attributes
import Time exposing ( millisToPosix)
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)

import Debug

--------------------------------------------------------------------------------
-- Model

type alias Model =
 { description : Field String String
 , electionType : Field (Maybe ElectionType) ElectionType
 , visibleVotes : Field Bool Bool
 , notification : Notification
 , title : Field String String
 }


initial : Model
initial =
  { title = Field.requiredString "Title"
  , description = Field.requiredString "Description"
  , electionType = Field.required "Election Type"
  , visibleVotes = Field.requiredBool "Vote Visibility" False
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
    , electionType = electionType
    , visibleVotes = visibleVotes
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
  | SetElectionType ElectionType
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
    SetElectionType val ->
      ( { model | electionType = Field.set (Just val) model.electionType }
      , Cmd.none
      )
    SetVisibleVotes val ->
      ( { model | visibleVotes = Field.set val model.visibleVotes }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html (Msg a))
view model =
  [ Input.text model.title SetTitle
  , Input.textarea model.description SetDescription
  , Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      , Html.Attributes.style "margin-top" "10px"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Input.radio model.electionType SetElectionType
              [ { name = "Simple Majority", value = SimpleMajority }
              , { name = "Majority Consensus", value = MajorityConsensus }
              ]
          ]
      , Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Input.checkbox model.visibleVotes SetVisibleVotes "All votes are visible" ]
      ]
  , Input.button "Save" ()
  ]
