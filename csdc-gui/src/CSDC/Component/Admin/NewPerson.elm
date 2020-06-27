module CSDC.Component.Admin.NewPerson exposing
  ( Model
  , initial
  , Msg
  , update
  , view
  )

import CSDC.API as API
import CSDC.Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)
import Field exposing (Field)
import Input
import Validation exposing (Validation)

import Element exposing (..)
import Element.Font as Font
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { name : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { name = Field.requiredString "Name"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputName String
  | APIMsg API.Msg
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputName name ->
      ( { model | name = Field.set name model.name }
      , Cmd.none
      )

    Submit ->
      case Validation.validate (Field.validate model.name) of
        Err e ->
          ( { model | notification = Notification.Error e }
          , Cmd.none
          )
        Ok name ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.insertPerson (Person name "ORCID" "")
          )

    APIMsg apimsg ->
      case apimsg of
        API.InsertPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok _ ->
              ( { initial | notification = Notification.Success }
              , Notification.reset Reset
              )
        _ ->
          (model, Cmd.none)

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> Element Msg
view model =
  column [ width <| fillPortion 2, padding 10, spacing 10 ] <|
    [ row
        [ Font.bold, Font.size 30 ]
        [ text "New Person" ]
    , Input.text
        { onChange = InputName
        , field = model.name
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
