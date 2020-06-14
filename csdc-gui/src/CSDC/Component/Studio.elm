module CSDC.Component.Studio exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  , Selected (..)
  , ViewSelected (..)
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Component.PreviewMessage as PreviewMessage
import CSDC.Component.PreviewReply as PreviewReply
import CSDC.Component.PreviewUnit as PreviewUnit
import CSDC.Input exposing (button)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Font as Font
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type Selected
  = SelectedNothing
  | SelectedUnit (Id Member)
  | SelectedInbox InboxId

type alias Model =
  { info : Maybe PersonInfo
  , panelUnits : Panel.Model (Id Member)
  , panelMessages : Panel.Model InboxId
  , notification : Notification
  , inbox : Inbox
  , selected : Selected
  }

initial : Model
initial =
  { info = Nothing
  , panelUnits = Panel.initial "Units"
  , panelMessages = Panel.initial "Messages"
  , notification = Notification.Empty
  , inbox = emptyInbox
  , selected = SelectedNothing
  }

setup : Id Person -> Cmd Msg
setup id =
  Cmd.map APIMsg <|
  Cmd.batch
    [ API.getPersonInfo id
    , API.personInbox id
    ]

--------------------------------------------------------------------------------
-- Update

type ViewSelected
  = ViewSelectedUnit (Id Unit)
  | ViewSelectedInbox InboxId MessageType ReplyType

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Member))
  | MessagesMsg (Panel.Msg InboxId)
  | CreateUnit
  | View ViewSelected

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UnitsMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelUnits = Panel.update m model.panelUnits
            , selected = SelectedUnit id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelUnits = Panel.update m model.panelUnits }
          , Cmd.none
          )

    MessagesMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelMessages = Panel.update m model.panelMessages
            , selected = SelectedInbox id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelMessages = Panel.update m model.panelMessages }
          , Cmd.none
          )

    View selected ->
      case selected of
        ViewSelectedUnit id ->
          (model, Cmd.none)

        ViewSelectedInbox inboxId mtype rtype ->
          case inboxId of
            MessageMemberId id ->
              let
                reply = Reply
                  { rtype = rtype
                  , mtype = mtype
                  , text = "Reply"
                  , status = NotSeen
                  , id = id
                  }
              in
                ( { model | selected = SelectedNothing }
                , Cmd.map APIMsg <| API.sendReplyMember reply
                )

            _ ->
             (model, Cmd.none)

    CreateUnit ->
      ( model
      , case model.info of
          Nothing -> Cmd.none
          Just info -> Cmd.map APIMsg <| API.createUnit info.id
      )

    APIMsg apimsg ->
      case apimsg of
        API.GetPersonInfo result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok info ->
              let
                pairs =
                  idMapToList info.members |>
                  List.map (\(uid,unit) -> (uid,unit.value.name))

                panelUnits = Panel.update (Panel.SetItems pairs) model.panelUnits
              in
                ( { model | info = Just info, panelUnits = panelUnits }
                , Cmd.none
                )

        API.PersonInbox result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok inbox ->
              let
                panelMessages =
                  Panel.update (Panel.SetItems <| inboxToPairs inbox) model.panelMessages
              in
                ( { model | inbox = inbox, panelMessages = panelMessages }
                , Cmd.none
                )

        API.SendReplyMember result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok _ ->
              case model.info of
                Nothing ->
                  ( model
                  , Cmd.none
                  )
                Just info ->
                  ( model
                  , setup info.id
                  )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  case model.info of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just info ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Studio" ]
      , row []
          [ text info.person.name ]
      , row []
          [ el [ Font.bold ] (text "ORCID ID: ")
          , newTabLink []
              { url = "https://orcid.org/" ++ info.person.orcid
              , label = text info.person.orcid
              }
          ]
      , row []
          [ text info.person.description ]
      , row []
          [ button CreateUnit "Create New Unit" ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map UnitsMsg <| Panel.view model.panelUnits
          , map MessagesMsg <| Panel.view model.panelMessages
          ]
      , case model.selected of
          SelectedNothing ->
            row [] []

          SelectedUnit id ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case idMapLookup id info.members of
                Nothing ->
                  [ text "Error." ]
                Just unit ->
                  PreviewUnit.view unit.value <|
                  View (ViewSelectedUnit unit.id)

          SelectedInbox inboxId ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case inboxId of
                MessageMemberId id ->
                  case idMapLookup id model.inbox.messageMember of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      PreviewMessage.view msg <|
                      (\mtype rtype -> View <| ViewSelectedInbox inboxId mtype rtype)

                ReplyMemberId id ->
                  case idMapLookup id model.inbox.replyMember of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      PreviewReply.view msg <|
                      -- XXX: Accept does not make sense
                      View (ViewSelectedInbox inboxId Invitation Accept)

                MessageSubpartId id ->
                  case idMapLookup id model.inbox.messageSubpart of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      PreviewMessage.view msg <|
                      (\mtype rtype -> View <| ViewSelectedInbox inboxId mtype rtype)

                ReplySubpartId id ->
                  case idMapLookup id model.inbox.replySubpart of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      PreviewReply.view msg <|
                      -- XXX: Accept does not make sense
                      View (ViewSelectedInbox inboxId Invitation Accept)

      ] ++
      Notification.view model.notification

--------------------------------------------------------------------------------
-- Helpers

type InboxId
  = MessageMemberId (Id (Message Member))
  | ReplyMemberId (Id (Reply Member))
  | MessageSubpartId (Id (Message Subpart))
  | ReplySubpartId (Id (Reply Subpart))

inboxToPairs : Inbox -> List (InboxId, String)
inboxToPairs inbox =
  let
    fmm (id, Message m) = (MessageMemberId id, m.text)
    frm (id, Reply r) = (ReplyMemberId id, r.text)
    fms (id, Message m) = (MessageSubpartId id, m.text)
    frs (id, Reply r) = (ReplySubpartId id, r.text)
  in
    List.map fmm (idMapToList inbox.messageMember) ++
    List.map frm (idMapToList inbox.replyMember) ++
    List.map fms (idMapToList inbox.messageSubpart) ++
    List.map frs (idMapToList inbox.replySubpart)
