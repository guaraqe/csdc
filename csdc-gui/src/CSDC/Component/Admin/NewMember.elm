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

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Maybe (Id Person)
  , unit : Maybe (Id Unit)
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , unit = Nothing
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
      let
        newPerson =
          case String.toInt str of
            Nothing -> Nothing
            Just n -> Just (Id n)
      in
        ( { model | person = newPerson }
        , Cmd.none
        )

    InputUnit str ->
      let
        newUnit =
          case String.toInt str of
            Nothing -> Nothing
            Just n -> Just (Id n)
      in
        ( { model | unit = newUnit }
        , Cmd.none
        )

    Submit ->
      case Maybe.map2 makeMember model.person model.unit of
        Nothing ->
          ( { model | notification = Notification.Error "Input wrong!" }
          , Cmd.none
          )
        Just member ->
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
        []
        { onChange = InputPerson
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Person")
        , text = Maybe.withDefault "" (Maybe.map idToString model.person)
        }

    , Input.text
        []
        { onChange = InputUnit
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Unit")
        , text = Maybe.withDefault "" (Maybe.map idToString model.unit)
        }

    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
