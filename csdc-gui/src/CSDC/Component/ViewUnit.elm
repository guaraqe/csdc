module CSDC.Component.ViewUnit exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  , ViewSelected (..)
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

type Selected
  = SelectedNothing
  | SelectedPerson (Id Member)
  | SelectedUnit (Id Subpart)

type ViewSelected
  = ViewSelectedPerson (Id Person)
  | ViewSelectedUnit (Id Unit)

type alias Model =
  { info : Maybe UnitInfo
  , panelChildren : Panel.Model (Id Subpart)
  , panelMembers : Panel.Model (Id Member)
  , notification : Notification
  , editName : EditableMode
  , editDescription : EditableMode
  , selected : Selected
  , invited : Maybe (Id Unit)
  , inbox : Inbox
  }

initial : Model
initial =
  { info = Nothing
  , panelChildren = Panel.initial "Sub-Units"
  , panelMembers = Panel.initial "Members"
  , notification = Notification.Empty
  , editName = EditableModeShow
  , editDescription = EditableModeShow
  , selected = SelectedNothing
  , invited = Nothing
  , inbox = emptyInbox
  }

setup : Id Unit -> Cmd Msg
setup id =
  Cmd.batch
    [ Cmd.map APIMsg <| API.getUnitInfo id
    , Cmd.map APIMsg <| API.unitInbox id
    ]

canEdit : Maybe (User PersonInfo) -> Model -> Bool
canEdit mid model =
  case mid of
    Nothing -> False
    Just Admin -> True
    Just (User pinfo) ->
      case model.info of
        Nothing -> False
        Just info ->
          case idMapFind (\w -> w.id == info.unit.chair) info.members of
            Nothing -> False
            Just member -> pinfo.id == member.id

isMember : Maybe (User PersonInfo) -> Model -> Maybe (WithId Person)
isMember mid model =
  case mid of
    Just (User pinfo) ->
      case model.info of
        Nothing -> Nothing
        Just info ->
          if idMapAny (\user -> user.id == pinfo.id) info.members
          then Nothing
          else Just { id = pinfo.id, value = pinfo.person }
    _ ->
      Nothing

isMemberPending : Maybe (User PersonInfo) -> Model -> Bool
isMemberPending mid model =
  let
    getMessagePerson (MessageInfo m) = getMemberPerson m.value
  in
  case mid of
    Just (User info) ->
      idMapAny (\m -> getMessagePerson m == info.id) model.inbox.messageMember
    _ ->
      False

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | SubpartsMsg (Panel.Msg (Id Subpart))
  | MembersMsg (Panel.Msg (Id Member))
  | EditName EditableMsg
  | EditDescription EditableMsg
  | View ViewSelected
  | MessageMember (WithId Person) (WithId Unit) MessageType
  | MessageSubpart PersonInfo UnitInfo MessageType
  | ViewAdmin (Id Unit)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SubpartsMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelChildren = Panel.update m model.panelChildren
            , selected = SelectedUnit id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelChildren = Panel.update m model.panelChildren }
          , Cmd.none
          )

    MembersMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelMembers = Panel.update m model.panelMembers
            , selected = SelectedPerson id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelMembers = Panel.update m model.panelMembers }
          , Cmd.none
          )

    EditName m ->
      case m of
        EditableEdit ->
          ( { model | editName = EditableModeEdit }
          , Cmd.none
          )
        EditableUpdate name ->
          let
            setName unit nam = { unit | name = nam }
          in
          ( { model
            | info =
                Maybe.map
                  (\info -> { info | unit = setName info.unit name })
                  model.info
            }
          , Cmd.none
          )
        EditableSave ->
          ( { model | editName = EditableModeShow }
          , case model.info of
              Nothing ->
                Cmd.none
              Just info ->
                Cmd.map APIMsg <| API.updateUnit info.id info.unit
          )

    EditDescription m ->
      case m of
        EditableEdit ->
          ( { model | editDescription = EditableModeEdit }
          , Cmd.none
          )
        EditableUpdate description ->
          let
            setDesc unit desc = { unit | description = desc }
          in
          ( { model
            | info =
                Maybe.map
                  (\info -> { info | unit = setDesc info.unit description })
                  model.info
            }
          , Cmd.none
          )
        EditableSave ->
          ( { model | editDescription = EditableModeShow }
          , case model.info of
              Nothing ->
                Cmd.none
              Just info ->
                Cmd.map APIMsg <| API.updateUnit info.id info.unit
          )

    View selected ->
      case selected of
        ViewSelectedPerson id ->
          ( model
          , Cmd.none
          )

        ViewSelectedUnit id ->
          ( model
          , Cmd.map APIMsg <| API.getUnitInfo id
          )

    MessageMember pid uid mtype ->
      ( model
      , Cmd.none
      )

    MessageSubpart pinfo uid mtype ->
      ( model
      , Cmd.none
      )

    ViewAdmin _ ->
      ( model
      , Cmd.none
      )

    APIMsg apimsg ->
      case apimsg of
        API.GetUnitInfo result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok info ->
              let
                pairsMembers =
                  idMapToList info.members |>
                  List.map (\(id,withid) -> (id, withid.value.name))

                panelMembers =
                  Panel.update (Panel.SetItems pairsMembers) model.panelMembers

                pairsChildren =
                  idMapToList info.children |>
                  List.map (\(id,withid) -> (id, withid.value.name))

                panelChildren =
                  Panel.update (Panel.SetItems pairsChildren) model.panelChildren
              in
                ( { model
                  | info = Just info
                  , panelMembers = panelMembers
                  , panelChildren = panelChildren
                  , selected = SelectedNothing
                  }
                , Cmd.none
                )

        API.UpdateUnit result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok unit ->
              ( { model | notification = Notification.Success }
              , Cmd.none
              )

        API.UnitInbox _ result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok inbox ->
              ( { model | inbox = inbox }
              , Cmd.none
              )

        API.SendMessageSubpart result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok _ ->
              ( model
              , case model.info of
                  Nothing -> Cmd.none
                  Just info -> Cmd.map APIMsg <| API.unitInbox info.id
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Maybe (User PersonInfo) -> Model -> List (Element Msg)
view mid model =
  case model.info of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just info ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Unit Viewer" ]
      , editableText
          { canEdit = canEdit mid model
          , mode = model.editName
          , label = "Name"
          , value = info.unit.name
          , event = EditName
          }
      , editableMultiline
          { canEdit = canEdit mid model
          , mode = model.editDescription
          , label = "Description"
          , value = info.unit.description
          , event = EditDescription
          }
      , row []
          [ text <| "Chair: " ++
              case idMapFind (\w -> w.id == info.unit.chair) info.members of
                Nothing -> "Loading..."
                Just withid -> withid.value.name
          ]
      , row [] <|
          if canEdit mid model
          then [ button (ViewAdmin info.id) "Admin" ]
          else []
      , row [] <|
          case isMember mid model of
            Nothing -> []
            Just wid ->
              if isMemberPending mid model
              then [ text "Your submission was sent." ]
              else
                let
                  msg = MessageMember wid { id = info.id, value = info.unit } Submission
                in
                  [ button msg "Become a member" ]
      , row [] <|
          case mid of
            Just (User pinfo) ->
              let
                msg = MessageSubpart pinfo info Invitation
              in
               [ button msg "Invite this unit to your unit" ]
            _ ->
               []
      , row [] <|
          case mid of
            Just (User pinfo) ->
              let
                msg = MessageSubpart pinfo info Submission
              in
               [ button msg "Make your unit a part of this unit" ]
            _ ->
               []
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map SubpartsMsg <| Panel.view model.panelChildren
          , map MembersMsg <| Panel.view model.panelMembers
          ]
      , case model.selected of
          SelectedNothing ->
            row [] []

          SelectedPerson id ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case idMapLookup id info.members of
                Nothing ->
                  [ text "Loading..." ]
                Just person ->
                  PreviewPerson.view person.value <|
                  View (ViewSelectedPerson person.id)

          SelectedUnit id ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case idMapLookup id info.children of
                Nothing ->
                  [ text "Loading..." ]
                Just subunit ->
                  PreviewUnit.view subunit.value <|
                  View (ViewSelectedUnit subunit.id)

      ] ++
      Notification.view model.notification
