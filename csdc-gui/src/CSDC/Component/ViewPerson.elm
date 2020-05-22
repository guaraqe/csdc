module CSDC.Component.ViewPerson exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
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

type alias Model =
  { id : Maybe (Id Person)
  , person : Maybe Person
  , member : IdMap Member Member
  , units : IdMap Member Unit
  , panelUnits : Panel.Model (Id Member)
  , notification : Notification
  }

initial : Model
initial =
  { id = Nothing
  , person = Nothing
  , member = idMapEmpty
  , units = idMapEmpty
  , panelUnits = Panel.initial "Units"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Member))
  | ViewSelected (Id Unit)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UnitsMsg m ->
      ( { model | panelUnits = Panel.update m model.panelUnits }
      , Cmd.none
      )

    ViewSelected _ -> (model, Cmd.none)

    APIMsg apimsg ->
      case apimsg of
        API.SelectPerson id result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok person ->
              ( { model | id = Just id, person = Just person }
              , Cmd.map (APIMsg) <|
                Cmd.batch
                  [ API.selectMemberPerson id
                  , API.unitsPerson id
                  ]
              )

        API.SelectMemberPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok member ->
              ( { model | member = member }
              , Cmd.none
              )

        API.UnitsPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok units ->
              let
                pairs =
                  idMapToList units |>
                  List.map (\(uid,unit) -> (uid,unit.name))

                panelUnits = Panel.update (Panel.SetItems pairs) model.panelUnits
              in
              ( { model | panelUnits = panelUnits, units = units }
              , Cmd.none
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  case model.person of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just person ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "View Person" ]
      , row []
          [ text person.name ]
      , row []
          [ el [ Font.bold ] (text "ORCID ID: ")
          , newTabLink []
              { url = "https://orcid.org/" ++ person.orcid
              , label = text person.orcid
              }
          ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ column
              [ width <| fillPortion 1 ]
              [ text person.description ]
          , map UnitsMsg <| Panel.view model.panelUnits
          ]
      , case Panel.getSelected model.panelUnits of
          Nothing ->
            row [] []
          Just id ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case
                Maybe.map2 pair
                  (idMapLookup id model.member)
                  (idMapLookup id model.units)
                of
                Nothing ->
                  [ text "Error." ]
                Just (Member member, unit) ->
                  PreviewUnit.view unit (ViewSelected member.unit)
      ] ++
      Notification.view model.notification
