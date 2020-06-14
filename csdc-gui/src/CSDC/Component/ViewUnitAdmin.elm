module CSDC.Component.ViewUnitAdmin exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Component.PreviewPerson as PreviewPerson
import CSDC.Component.PreviewUnit as PreviewUnit
import CSDC.Input exposing (..)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type MemberId
  = MemberMessage (Id (Message Member))
  | MemberReply (Id (Reply Member))

type SubpartId
  = SubpartMessage (Id (Message Subpart))
  | SubpartReply (Id (Reply Subpart))

type alias Model =
  { id : Maybe (Id Unit)
  , unit : Maybe Unit
  , inbox : Inbox
  , panelMember : Panel.Model MemberId
  , panelSubpart : Panel.Model SubpartId
  , notification : Notification
  }

initial : Model
initial =
  { id = Nothing
  , unit = Nothing
  , inbox = emptyInbox
  , panelMember = Panel.initial "Member Messages"
  , panelSubpart = Panel.initial "Subpart Messages"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | PanelMemberMsg (Panel.Msg MemberId)
  | PanelSubpartMsg (Panel.Msg SubpartId)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PanelMemberMsg m ->
      ( { model
        | panelMember = Panel.update m model.panelMember
        }
      , Cmd.none
      )

    PanelSubpartMsg m ->
      ( { model
        | panelSubpart = Panel.update m model.panelSubpart
        }
      , Cmd.none
      )

    APIMsg apimsg ->
      case apimsg of
        API.UnitInbox uid result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok inbox ->
              let
                memberPairs =
                  let
                    fmm (id, Message m) = (MemberMessage id, m.text)
                    frm (id, Reply r) = (MemberReply id, r.text)
                  in
                    List.map fmm (idMapToList inbox.messageMember) ++
                    List.map frm (idMapToList inbox.replyMember)

                subpartPairs =
                  let
                    fms (id, Message m) = (SubpartMessage id, m.text)
                    frs (id, Reply r) = (SubpartReply id, r.text)
                  in
                    List.map fms (idMapToList inbox.messageSubpart) ++
                    List.map frs (idMapToList inbox.replySubpart)

                panelMember =
                  Panel.update (Panel.SetItems memberPairs) model.panelMember

                panelSubpart =
                  Panel.update (Panel.SetItems subpartPairs) model.panelSubpart

              in
                ( { model
                  | id = Just uid
                  , panelMember = panelMember
                  , panelSubpart = panelSubpart
                  }
                , Cmd.none
                )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Maybe (User PersonInfo) -> Model -> List (Element Msg)
view mid model =
  case model.id of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just id ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Unit Admin" ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map PanelMemberMsg <| Panel.view model.panelMember
          , map PanelSubpartMsg <| Panel.view model.panelSubpart
          ]
      ] ++
      Notification.view model.notification
