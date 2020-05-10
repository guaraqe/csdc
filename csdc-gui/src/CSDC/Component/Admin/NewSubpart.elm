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

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { child : Maybe (Id Unit)
  , parent : Maybe (Id Unit)
  , notification : Notification
  }

initial : Model
initial =
  { child = Nothing
  , parent = Nothing
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
      let
        newPerson =
          case String.toInt str of
            Nothing -> Nothing
            Just n -> Just (Id n)
      in
        ( { model | child = newPerson }
        , Cmd.none
        )

    InputParent str ->
      let
        newUnit =
          case String.toInt str of
            Nothing -> Nothing
            Just n -> Just (Id n)
      in
        ( { model | parent = newUnit }
        , Cmd.none
        )

    Submit ->
      case Maybe.map2 Subpart model.child model.parent of
        Nothing ->
          ( { model | notification = Notification.Error "Input wrong!" }
          , Cmd.none
          )
        Just member ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.insertSubpart member
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
        []
        { onChange = InputChild
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Child")
        , text = Maybe.withDefault "" (Maybe.map idToString model.child)
        }

    , Input.text
        []
        { onChange = InputParent
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Parent")
        , text = Maybe.withDefault "" (Maybe.map idToString model.parent)
        }

    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
