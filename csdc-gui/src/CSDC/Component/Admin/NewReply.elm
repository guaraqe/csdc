module CSDC.Component.Admin.NewReply exposing
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
  { message : Field String (Id (Message Member))
  , replyType : Field (Maybe ReplyType) ReplyType
  , messageType : Field (Maybe MessageType) MessageType
  , replyStatus : Field (Maybe ReplyStatus) ReplyStatus
  , notification : Notification
  }

initial : Model
initial =
  { message = Field.requiredId "Message"
  , replyType = Field.required "Reply Type"
  , messageType = Field.required "Message Type"
  , replyStatus = Field.required "Reply Status"
  , notification = Notification.Empty
  }

validate : Model -> Result (List String) (Reply Member)
validate model =
  let
    result =
      Validation.valid makeReply
        |> Validation.andMap (Field.validate model.replyType)
        |> Validation.andMap (Field.validate model.messageType)
        |> Validation.andMap (Validation.valid "Reply")
        |> Validation.andMap (Field.validate model.replyStatus)
        |> Validation.andMap (Field.validate model.message)
  in
    Validation.validate result

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputMessage String
  | APIMsg API.Msg
  | InputMessageType MessageType
  | InputReplyType ReplyType
  | InputReplyStatus ReplyStatus
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputMessage str ->
      ( { model | message = Field.set str model.message }
      , Cmd.none
      )

    Submit ->
      case validate model of
        Err e ->
          ( { model | notification = Notification.Error e }
          , Cmd.none
          )
        Ok reply ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.sendReplyMember reply
          )

    InputReplyStatus replyStatus ->
      ( { model | replyStatus = Field.set (Just replyStatus) model.replyStatus }
      , Cmd.none
      )

    InputReplyType replyType ->
      ( { model | replyType = Field.set (Just replyType) model.replyType }
      , Cmd.none
      )

    InputMessageType messageType ->
      ( { model | messageType = Field.set (Just messageType) model.messageType }
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

        API.SendReplyMember result ->
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
        [ text "New Reply" ]
    , selectReplyType model
    , selectReplyStatus model
    , Input.text
        { onChange = InputMessage
        , field = model.message
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification

selectReplyType : Model -> Element Msg
selectReplyType model =
  Input.radio
    { onChange = InputReplyType
    , field = model.replyType
    , options =
        [ (Accept, "Accept")
        , (Reject, "Reject")
        ]
    }

selectMessageType : Model -> Element Msg
selectMessageType model =
  Input.radio
    { onChange = InputMessageType
    , field = model.messageType
    , options =
        [ (Invitation, "Invitation")
        , (Submission, "Submission")
        ]
    }

selectReplyStatus : Model -> Element Msg
selectReplyStatus model =
  Input.radio
    { onChange = InputReplyStatus
    , field = model.replyStatus
    , options =
        [ (Seen, "Seen")
        , (NotSeen, "Not Seen")
        ]
    }
