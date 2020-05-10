module CSDC.Component.ViewUnit exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
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

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | SubpartsMsg (Panel.Msg (Id Subpart))
  | MembersMsg (Panel.Msg (Id Member))
  | EditName EditableMsg
  | EditDescription EditableMsg
  | ViewSelected (Id Unit)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SubpartsMsg m ->
      ( { model | panelSubparts = Panel.update m model.panelSubparts }
      , Cmd.none
      )

    MembersMsg m ->
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

    ViewSelected id ->
      ( model
      , Cmd.map APIMsg <| API.selectUnit id
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
              ( { model | id = Just id, unit = Just unit }
              , Cmd.batch
                  [ Cmd.map APIMsg <| API.getUnitMembers id
                  , Cmd.map APIMsg <| API.getUnitSubparts id
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

        API.GetUnitSubparts result ->
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
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map SubpartsMsg <| Panel.view model.panelSubparts
          , map MembersMsg <| Panel.view model.panelMembers
          ]
      , case Panel.getSelected model.panelSubparts of
          Nothing ->
            row [] []
          Just id ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case idMapLookup id model.subparts of
                Nothing ->
                  [ text "Loading..." ]
                Just subunit ->
                  PreviewUnit.view subunit.value (ViewSelected subunit.id)
      ] ++
      Notification.view model.notification
