module CSDC.Component.Studio exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Input exposing (button)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
  , panelMessages : Panel.Model Int -- todo: actually implement messages
  , notification : Notification
  }

initial : Model
initial =
  { id = Nothing
  , person = Nothing
  , member = idMapEmpty
  , units = idMapEmpty
  , panelUnits = Panel.initial "Units"
  , panelMessages = Panel.initial "Messages"
  , notification = Notification.Empty
  }

setup : Id Person -> Cmd Msg
setup id =
  Cmd.batch
    [ Cmd.map APIMsg <| API.selectPerson id
    , Cmd.map APIMsg <| API.selectMemberPerson id
    , Cmd.map APIMsg <| API.unitsPerson id
    ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Member))
  | MessagesMsg (Panel.Msg Int)
  | CreateUnit
  | ViewSelected (Id Unit)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UnitsMsg m ->
      ( { model | panelUnits = Panel.update m model.panelUnits }
      , Cmd.none
      )

    MessagesMsg m ->
      ( { model | panelMessages = Panel.update m model.panelMessages }
      , Cmd.none
      )

    ViewSelected _ -> (model, Cmd.none)

    CreateUnit ->
      ( model
      , case model.id of
          Nothing -> Cmd.none
          Just id -> Cmd.map APIMsg <| API.createUnit id
      )

    APIMsg apimsg ->
      case apimsg of
        API.SelectPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok person ->
              ( { model | person = Just person }
              , Cmd.none
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
          [ text "Studio" ]
      , row []
          [ text person.name ]
      , row []
          [ el [ Font.bold ] (text "ORCID ID: ")
          , newTabLink []
              { url = "https://orcid.org/" ++ person.orcid
              , label = text person.orcid
              }
          ]
      , row []
          [ text person.description ]
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
                  [ column
                     [ height fill
                     , width fill
                     , spacing 10
                     , Border.width 1
                     , Border.color <| rgb255 92 99 118
                     , Border.rounded 5
                     ]
                     [ row
                        [ spacing 10
                        , alignTop
                        , Background.color <| rgb255 92 99 118
                        , padding 10
                        , width fill
                        ]
                        [ column
                            [ Font.size 24 ]
                            [ text unit.name ]
                        , column
                            [ alignRight ]
                            [ button (ViewSelected member.unit) "View Unit" ]
                        ]
                     , row
                        [ height fill
                        , padding 10
                        ]
                        [ text unit.description
                        ]
                     ]
                  ]
      ] ++
      Notification.view model.notification
