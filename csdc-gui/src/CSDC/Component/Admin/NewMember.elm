module CSDC.Component.Admin.NewMember exposing
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
  { person : Field String (Id Person)
  , unit : Field String (Id Unit)
  , notification : Notification
  }

initial : Model
initial =
  { person = Field.requiredId "Person"
  , unit = Field.requiredId "Unit"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputPerson String
  | InputUnit String
  | APIMsg API.Msg
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputPerson str ->
      ( { model | person = Field.set str model.person }
      , Cmd.none
      )

    InputUnit str ->
      ( { model | unit = Field.set str model.unit }
      , Cmd.none
      )

    Submit ->
      let
        result =
          Validation.valid makeMember
            |> Validation.andMap (Field.validate model.person)
            |> Validation.andMap (Field.validate model.unit)
      in
        case Validation.validate result of
          Err e ->
            ( { model | notification = Notification.Error e }
            , Cmd.none
            )
          Ok member ->
            ( { model | notification = Notification.Processing }
            , Cmd.map APIMsg <| API.insertMember member
            )

    APIMsg apimsg ->
      case apimsg of
        API.InsertMember result ->
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
        [ text "New Member" ]

    , Input.text
        { onChange = InputPerson
        , field = model.person
        }

    , Input.text
        { onChange = InputUnit
        , field = model.unit
        }

    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
