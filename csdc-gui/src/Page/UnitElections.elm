module Page.UnitElections exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxElection as BoxElection
import UI.Column as Column
import UI.Modal as Modal
import Notification exposing (Notification)
import Page as Page exposing (UnitTab (..))
import Types exposing (..)

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { elections : List ElectionInfo
  , electionFormOpen : Bool
  , voteFormOpen : Bool
  , selected : Maybe (Id Election)
  , notification : Notification
  }

initial : Model
initial =
  { elections = []
  , electionFormOpen = False
  , voteFormOpen = False
  , selected = Nothing
  , notification = Notification.Empty
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map SetElections <| API.getElections id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetElections (API.Response (List ElectionInfo))
  | ElectionFormOpen
  | ElectionFormClose
  | VoteFormOpen (Id Election)
  | VoteFormClose
  | ResetNotification

update : Page.Info -> UnitInfo -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo info msg model =
  let
    onSuccess = Notification.withResponse pageInfo ResetNotification model
  in
  case msg of
    SetElections result -> onSuccess result <| \elections ->
      ( { model | elections = elections }
      , Cmd.none
      )

    ElectionFormOpen ->
      ( { model | electionFormOpen = True }
      , Cmd.none
      )

    ElectionFormClose ->
      ( { model | electionFormOpen = False }
      , Cmd.none
      )

    VoteFormOpen electionId ->
      ( { model | voteFormOpen = True, selected = Just electionId }
      , Cmd.none
      )

    VoteFormClose ->
      ( { model | voteFormOpen = False, selected = Nothing }
      , Cmd.none
      )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : UnitInfo -> Model -> List (Html Msg)
view unit model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Column.view "Elections"
            ( if unit.isAdmin
              then [smallButton]
              else []
            ) <|
            viewElections model.elections
          ]
      ]

  , Modal.view model.electionFormOpen ElectionFormClose <|
      Html.text "Election form here."

  , Modal.view model.voteFormOpen VoteFormClose <|
      Html.text "Vote form here."

  ] ++
  Notification.view model.notification

viewElections : List ElectionInfo -> List (Html Msg)
viewElections = List.map (BoxElection.view VoteFormOpen)

smallButton : Html Msg
smallButton =
  Html.button
    [ Html.Attributes.class "button is-success is-small"
    , Html.Events.onClick ElectionFormOpen
    ]
    [ Html.text "Create Election"
    ]
