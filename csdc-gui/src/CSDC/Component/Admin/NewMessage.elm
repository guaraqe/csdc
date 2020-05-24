module CSDC.Component.Admin.NewMessage exposing
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
  , messageType : Maybe MessageType
  , messageStatus : Maybe MessageStatus
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , unit = Nothing
  , messageType = Nothing
  , messageStatus = Nothing
  , notification = Notification.Empty
  }

validate : Model -> Maybe (Message Member)
validate model =
  model.person |> Maybe.andThen
    (\person ->
      model.unit |> Maybe.andThen
        (\unit ->
          model.messageType |> Maybe.andThen
            (\messageType ->
              model.messageStatus |> Maybe.andThen
                (\messageStatus ->
                  Just <|
                  makeMessage messageType "Message" messageStatus <|
                  makeMember person unit
                )
            )
        )
    )

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputPerson String
  | InputUnit String
  | APIMsg API.Msg
  | InputMessageType MessageType
  | InputMessageStatus MessageStatus
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
      case validate model of
        Nothing ->
          ( { model | notification = Notification.Error "Input wrong!" }
          , Cmd.none
          )
        Just message ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.sendMessageMember message
          )

    InputMessageStatus messageStatus ->
      ( { model | messageStatus = Just messageStatus }
      , Cmd.none
      )

    InputMessageType messageType ->
      ( { model | messageType = Just messageType }
      , Cmd.none
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

        API.SendMessageMember result ->
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
        [ text "New Message" ]
    , selectMessageType model
    , selectMessageStatus model
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

selectMessageType : Model -> Element Msg
selectMessageType model =
  Input.radioRow
    [ padding 10
    , spacing 20
    ]
    { onChange = InputMessageType
    , selected = model.messageType
    , label = Input.labelAbove [] (text "Message Type")
    , options =
        [ Input.option Invitation (text "Invitation")
        , Input.option Submission (text "Submission")
        ]
    }

selectMessageStatus : Model -> Element Msg
selectMessageStatus model =
  Input.radioRow
    [ padding 10
    , spacing 20
    ]
    { onChange = InputMessageStatus
    , selected = model.messageStatus
    , label = Input.labelAbove [] (text "Message Status")
    , options =
        [ Input.option Waiting (text "Waiting")
        , Input.option Accepted (text "Accepted")
        , Input.option Rejected (text "Rejected")
        ]
    }
