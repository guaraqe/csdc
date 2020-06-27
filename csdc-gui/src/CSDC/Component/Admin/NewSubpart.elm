module CSDC.Component.Admin.NewSubpart exposing
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
  { child : Field String (Id Unit)
  , parent : Field String (Id Unit)
  , notification : Notification
  }

initial : Model
initial =
  { child = Field.requiredId "Child"
  , parent = Field.requiredId "Parent"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputChild String
  | InputParent String
  | APIMsg API.Msg
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputChild str ->
      ( { model | child = Field.set str model.child }
      , Cmd.none
      )

    InputParent str ->
      ( { model | parent = Field.set str model.parent }
      , Cmd.none
      )

    Submit ->
      let
        result =
          Validation.valid Subpart
            |> Validation.andMap (Field.validate model.child)
            |> Validation.andMap (Field.validate model.parent)
      in
        case Validation.validate result of
          Err e ->
            ( { model | notification = Notification.Error e }
            , Cmd.none
            )
          Ok subpart ->
            ( { model | notification = Notification.Processing }
            , Cmd.map APIMsg <| API.insertSubpart subpart
            )

    APIMsg apimsg ->
      case apimsg of
        API.InsertSubpart result ->
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
        [ text "New Subpart" ]

    , Input.text
        { onChange = InputChild
        , field = model.child
        }

    , Input.text
        { onChange = InputParent
        , field = model.parent
        }

    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
