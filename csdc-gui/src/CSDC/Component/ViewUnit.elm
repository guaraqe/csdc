module CSDC.Component.ViewUnit exposing
  ( Model
  , initial
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
  { id : Maybe (Id Unit)
  , unit : Maybe Unit
  , members : IdMap Member (WithId Person)
  , subparts : IdMap Subpart (WithId Unit)
  , panelSubparts : Panel.Model (Id Subpart)
  , panelMembers : Panel.Model (Id Member)
  , notification : Notification
  , editName : EditableMode
  , editDescription : EditableMode
  , selected : Selected
  , inbox : Inbox
  }

initial : Model
initial =
  { id = Nothing
  , unit = Nothing
  , subparts = idMapEmpty
  , members = idMapEmpty
  , panelSubparts = Panel.initial "Sub-Units"
  , panelMembers = Panel.initial "Members"
  , notification = Notification.Empty
  , editName = EditableModeShow
  , editDescription = EditableModeShow
  , selected = SelectedNothing
  , inbox = emptyInbox
  }

canEdit : Maybe UserId -> Model -> Bool
canEdit mid model =
  case mid of
    Nothing -> False
    Just Admin -> True
    Just (User id) ->
      case model.unit of
        Nothing -> False
        Just unit ->
          case idMapLookup unit.chair model.members of
            Nothing -> False
            Just member -> id == member.id

isMember : Maybe UserId -> Model -> Maybe (Id Person)
isMember mid model =
  case mid of
    Just (User id) ->
      if idMapAny (\user -> user.id == id) model.members
      then Nothing
      else Just id
    _ ->
      Nothing

isPending : Maybe UserId -> Model -> Bool
isPending mid model =
  let
    getMessagePerson (Message m) = getMemberPerson m.value
  in
  case mid of
    Just (User id) ->
      idMapAny (\m -> getMessagePerson m == id) model.inbox.messageMember
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
  | SendSubmission (Id Person)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SubpartsMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelSubparts = Panel.update m model.panelSubparts
            , selected = SelectedUnit id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelSubparts = Panel.update m model.panelSubparts }
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
          ( { model
            | unit = Maybe.map (\unit -> { unit | name = name }) model.unit
            }
          , Cmd.none
          )
        EditableSave ->
          ( { model | editName = EditableModeShow }
          , case Maybe.map2 pair model.id model.unit of
              Nothing ->
                Cmd.none
              Just (id, unit) ->
                Cmd.map APIMsg <| API.updateUnit id unit
          )

    EditDescription m ->
      case m of
        EditableEdit ->
          ( { model | editDescription = EditableModeEdit }
          , Cmd.none
          )
        EditableUpdate description ->
          ( { model
            | unit = Maybe.map (\unit -> { unit | description = description }) model.unit
            }
          , Cmd.none
          )
        EditableSave ->
          ( { model | editDescription = EditableModeShow }
          , case Maybe.map2 pair model.id model.unit of
              Nothing ->
                Cmd.none
              Just (id, unit) ->
                Cmd.map APIMsg <| API.updateUnit id unit
          )

    View selected ->
      case selected of
        ViewSelectedPerson id ->
          ( model
          , Cmd.map APIMsg <| API.selectPerson id
          )

        ViewSelectedUnit id ->
          ( model
          , Cmd.map APIMsg <| API.selectUnit id
          )

    SendSubmission personId ->
      case model.id of
        Nothing -> (model, Cmd.none)
        Just unitId ->
          let
            submission =
              Message
                { mtype = Submission
                , text = "I want to be part of the unit."
                , status = Waiting
                , value = makeMember personId unitId
                }
          in
            ( model
            , Cmd.map APIMsg <| API.sendMessageMember submission
            )

    APIMsg apimsg ->
      case apimsg of
        API.SelectUnit id result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok unit ->
              ( { model
                | id = Just id, unit = Just unit, selected = SelectedNothing
                }
              , Cmd.batch
                  [ Cmd.map APIMsg <| API.getUnitMembers id
                  , Cmd.map APIMsg <| API.getUnitChildren id
                  , Cmd.map APIMsg <| API.unitInbox id
                  ]
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

        API.UnitInbox result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok inbox ->
              ( { model | inbox = inbox }
              , Cmd.none
              )

        API.GetUnitMembers result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok members ->
              let
                pairs =
                  idMapToList members |>
                  List.map (\(id,withid) -> (id, withid.value.name))

                panelMembers = Panel.update (Panel.SetItems pairs) model.panelMembers
              in
                ( { model | members = members, panelMembers = panelMembers }
                , Cmd.none
                )

        API.GetUnitChildren result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok subparts ->
              let
                pairs =
                  idMapToList subparts |>
                  List.map (\(id,withid) -> (id, withid.value.name))

                panelSubparts = Panel.update (Panel.SetItems pairs) model.panelSubparts
              in
                ( { model | subparts = subparts, panelSubparts = panelSubparts }
                , Cmd.none
                )

        API.SendMessageMember result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok _ ->
              ( model
              , case model.id of
                  Nothing -> Cmd.none
                  Just id -> Cmd.map APIMsg <| API.unitInbox id
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Maybe UserId -> Model -> List (Element Msg)
view mid model =
  case model.unit of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just unit ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Unit Viewer" ]
      , editableText
          { canEdit = canEdit mid model
          , mode = model.editName
          , label = "Name"
          , value = unit.name
          , event = EditName
          }
      , editableMultiline
          { canEdit = canEdit mid model
          , mode = model.editDescription
          , label = "Description"
          , value = unit.description
          , event = EditDescription
          }
      , row []
          [ text <| "Chair: " ++
              case idMapLookup unit.chair model.members of
                Nothing -> "Loading..."
                Just withid -> withid.value.name
          ]
      , row [] <|
          case isMember mid model of
            Nothing -> []
            Just id ->
              if isPending mid model
              then [ text "Your submission was sent." ]
              else [ button (SendSubmission id) "Become a member" ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map SubpartsMsg <| Panel.view model.panelSubparts
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
              case idMapLookup id model.members of
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
              case idMapLookup id model.subparts of
                Nothing ->
                  [ text "Loading..." ]
                Just subunit ->
                  PreviewUnit.view subunit.value <|
                  View (ViewSelectedUnit subunit.id)

      ] ++
      Notification.view model.notification
