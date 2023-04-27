module Form.UnitElections exposing
  ( Model
  , initial
  , subscriptions
  , Msg
  , updateWith
  , view
  )

import API as API

import Form
import Page
import Html exposing (Html)
import Html.Attributes
import Time exposing (Posix, millisToPosix)
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)
import SingleDatePicker exposing (DatePicker)
import Task

import Debug

--------------------------------------------------------------------------------
-- Model

type alias Model =
 { title : Field String String
 , description : Field String String
 , electionType : Field (Maybe ElectionType) ElectionType
 , visibleVotes : Field Bool Bool
 , choices : Input.TextFieldModel ElectionChoice
 , endingAt : Field (Maybe Posix) Posix
 , datePicker : DatePicker
 , notification : Notification
 }

choiceField : Field String String
choiceField = Field.requiredString ""

initial : Model
initial =
  { title = Field.requiredString "Title"
  , description = Field.requiredString "Description"
  , electionType = Field.required "Election Type"
  , visibleVotes = Field.requiredBool "Vote Visibility" False
  , choices = Field.requiredStringList "Choices" 2 choiceField
  , endingAt = Field.required "Ending At"
  , datePicker = SingleDatePicker.init
  , notification = Notification.Empty
  }

datePickerSettings : Time.Zone -> (ModelMsg -> msg) -> SingleDatePicker.Settings msg
datePickerSettings zone toMsg =
  SingleDatePicker.defaultSettings zone (toMsg << SetDateAndTime)

subscriptions : Time.Zone -> Model -> Sub (Msg a)
subscriptions zone model =
  SingleDatePicker.subscriptions
    (datePickerSettings zone Form.ModelMsg)
    (Form.ModelMsg << SetDateAndTime)
    model.datePicker

reload : Model -> Model
reload model =
  { model
  | title = Field.reload model.title
  , description = Field.reload model.description
  , electionType = Field.reload model.electionType
  , choices = Field.reload model.choices
  , visibleVotes = Field.reload model.visibleVotes
  , endingAt = Field.reload model.endingAt
  }

-- Also unit update...
parse : Model -> Maybe NewElection
parse model = Result.toMaybe <|
  Field.with model.title <| \title ->
  Field.with model.description <| \description ->
  Field.with model.electionType <| \electionType ->
  Field.with model.visibleVotes <| \visibleVotes ->
  Field.with model.choices <| \choices ->
  Field.with model.endingAt <| \endingAt ->
  Ok
    { title = title
    , description = description
    , choices = choices
    , electionType = electionType
    , visibleVotes = visibleVotes
    , endingAt = endingAt
    }

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { pageInfo : Page.Info
  , request : NewElection -> Cmd (API.Response a)
  , finish : a -> Cmd (Msg a)
  , zone : Time.Zone
  , now : Posix
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update config.zone config.now
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
  | SetChoices Input.TextListAction
  | SetDateAndTime (DatePicker, Maybe Posix)
  | OpenDateAndTime

update : Time.Zone -> Posix -> ModelMsg -> Model -> (Model, Cmd ModelMsg)
update zone now msg model =
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
    SetChoices val ->
      ( { model
        | choices = Input.textListUpdate choiceField val model.choices
        }
      , Cmd.none
      )
    SetDateAndTime (datePicker, mposix) ->
      let
        s = "Now: " ++ viewPosixAt zone now ++
            "\nNew: " ++
               case mposix of
                 Nothing -> "?"
                 Just t -> viewPosixAt zone t
      in
      ( { model
        | datePicker = Debug.log s datePicker
        , endingAt = case mposix of
            Nothing -> model.endingAt
            Just _ -> Field.set mposix model.endingAt
        }
      , Cmd.none
      )
    OpenDateAndTime ->
      let
        -- For some reason, if this is not done, we cannot select time
        selected = case Field.raw model.endingAt of
          Nothing -> Just now
          Just a -> Just a

        datePicker =
          SingleDatePicker.openPicker
            (datePickerSettings zone identity)
            now
            selected
            model.datePicker
      in
        ( { model | datePicker = datePicker }
        , Cmd.none
        )

--------------------------------------------------------------------------------
-- View

view : Time.Zone -> Model -> List (Html (Msg a))
view zone model =
  [ Input.text model.title SetTitle
  , Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      , Html.Attributes.style "margin-top" "10px"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Input.dateAndTime
              model.endingAt
              zone
              (datePickerSettings zone Form.ModelMsg)
              model.datePicker
              OpenDateAndTime
          , Input.radio model.electionType SetElectionType
              [ { name = "Simple Majority", value = SimpleMajority }
              , { name = "Majority Consensus", value = MajorityConsensus }
              ]
          , Input.checkbox model.visibleVotes SetVisibleVotes "All votes are visible"
          ]
      , Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Input.textList model.choices SetChoices ]
      ]
  , Input.textarea model.description SetDescription
  , Input.button "Save" ()
  ]
