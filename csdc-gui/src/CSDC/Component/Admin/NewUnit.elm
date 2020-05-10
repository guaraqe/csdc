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

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { chair : Maybe (Id Person)
  , notification : Notification
  }

initial : Model
initial =
  { chair = Nothing
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
    InputId chair ->
      ( { model | chair = idFromString chair }
      , Cmd.none
      )

    Submit ->
      case model.chair of
        Nothing ->
          ( { model | notification = Notification.Error "Chair must not be empty!" }
          , Cmd.none
          )
        Just id ->
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
        []
        { onChange = InputId
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Chair Id")
        , text = Maybe.withDefault "" (Maybe.map idToString model.chair)
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification
