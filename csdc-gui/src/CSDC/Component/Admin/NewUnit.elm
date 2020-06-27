module CSDC.Component.Admin.NewUnit exposing
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
  { chair : Field String (Id Person)
  , notification : Notification
  }

initial : Model
initial =
  { chair = Field.requiredId "Chair"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputId String
  | APIMsg API.Msg
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputId str ->
      ( { model | chair = Field.set str model.chair }
      , Cmd.none
      )

    Submit ->
      case Validation.validate (Field.validate model.chair) of
        Err e ->
          ( { model | notification = Notification.Error e }
          , Cmd.none
          )
        Ok id ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.createUnit id
          )

    APIMsg apimsg ->
      case apimsg of
        API.CreateUnit result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok memberWithId ->
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
        [ text "New Unit" ]
    , Input.text
        { onChange = InputId
        , field = model.chair
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
