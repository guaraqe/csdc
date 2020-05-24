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

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { message : Maybe (Id (Message Member))
  , replyType : Maybe ReplyType
  , replyStatus : Maybe ReplyStatus
  , notification : Notification
  }

initial : Model
initial =
  { message = Nothing
  , replyType = Nothing
  , replyStatus = Nothing
  , notification = Notification.Empty
  }

validate : Model -> Maybe (Reply Member)
validate model =
  model.message |> Maybe.andThen
    (\message ->
      model.replyType |> Maybe.andThen
        (\replyType ->
          model.replyStatus |> Maybe.andThen
            (\replyStatus ->
              Just <|
              makeReply replyType "Reply" replyStatus message
            )
        )
    )

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputMessage String
  | APIMsg API.Msg
  | InputReplyType ReplyType
  | InputReplyStatus ReplyStatus
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputMessage str ->
      let
        message =
          case String.toInt str of
            Nothing -> Nothing
            Just n -> Just (Id n)
      in
        ( { model | message = message }
        , Cmd.none
        )

    Submit ->
      case validate model of
        Nothing ->
          ( { model | notification = Notification.Error "Input wrong!" }
          , Cmd.none
          )
        Just reply ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.sendReplyMember reply
          )

    InputReplyStatus replyStatus ->
      ( { model | replyStatus = Just replyStatus }
      , Cmd.none
      )

    InputReplyType replyType ->
      ( { model | replyType = Just replyType }
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
        []
        { onChange = InputMessage
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Message")
        , text = Maybe.withDefault "" (Maybe.map idToString model.message)
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification

selectReplyType : Model -> Element Msg
selectReplyType model =
  Input.radioRow
    [ padding 10
    , spacing 20
    ]
    { onChange = InputReplyType
    , selected = model.replyType
    , label = Input.labelAbove [] (text "Reply Type")
    , options =
        [ Input.option Accept (text "Accept")
        , Input.option Reject (text "Reject")
        ]
    }

selectReplyStatus : Model -> Element Msg
selectReplyStatus model =
  Input.radioRow
    [ padding 10
    , spacing 20
    ]
    { onChange = InputReplyStatus
    , selected = model.replyStatus
    , label = Input.labelAbove [] (text "Reply Status")
    , options =
        [ Input.option Seen (text "Seen")
        , Input.option NotSeen (text "Not Seen")
        ]
    }
