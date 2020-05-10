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

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { name : Maybe String
  , notification : Notification
  }

initial : Model
initial =
  { name = Nothing
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
      let
        newName =
          if String.isEmpty name
          then Nothing
          else Just name
      in
        ( { model | name = newName }
        , Cmd.none
        )

    Submit ->
      case model.name of
        Nothing ->
          ( { model | notification = Notification.Error "Name must not be empty!" }
          , Cmd.none
          )
        Just name ->
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
        []
        { onChange = InputName
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Name")
        , text = Maybe.withDefault "" model.name
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
