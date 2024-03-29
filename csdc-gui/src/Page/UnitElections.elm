module Page.UnitElections exposing
  ( Model
  , setup
  , subscriptions
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxElection as BoxElection
import UI.Column as Column
import UI.Modal as Modal
import Form.UnitElections as ElectionsForm
import Form.UnitVote as VoteForm
import Notification exposing (Notification)
import Page as Page exposing (UnitTab (..))
import Types exposing (..)

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import Time
import Form

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { elections : List ElectionInfo
  , electionForm : ElectionsForm.Model
  , electionFormOpen : Bool
  , voteForm : VoteForm.Model
  , voteFormOpen : Bool
  , selected : Maybe (Id Election)
  , now : Time.Posix
  , notification : Notification
  }

initial : Model
initial =
  { elections = []
  , electionForm = ElectionsForm.initial
  , electionFormOpen = False
  , voteForm = VoteForm.initial
  , voteFormOpen = False
  , selected = Nothing
  , now = Time.millisToPosix 1
  , notification = Notification.Empty
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map SetElections <| API.getElections id
  , Task.perform SetInitialTime Time.now
  ]

subscriptions : Time.Zone -> Model -> Sub Msg
subscriptions zone model =
  Sub.map ElectionFormMsg <|
  ElectionsForm.subscriptions zone model.electionForm

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetElections (API.Response (List ElectionInfo))
  | SelectElection (Id Election)
  | ElectionFormMsg (ElectionsForm.Msg (Id Election))
  | ElectionFormOpen
  | ElectionFormClose
  | VoteFormMsg (VoteForm.Msg (Id Vote))
  | VoteFormOpen
  | VoteFormClose
  | SetInitialTime Time.Posix
  | ResetNotification

update : Time.Zone -> Page.Info -> UnitInfo -> Msg -> Model -> (Model, Cmd Msg)
update zone pageInfo info msg model =
  let
    reload = Page.goTo pageInfo (Page.Unit Page.UnitElections info.id)

    onSuccess = Notification.withResponse pageInfo ResetNotification model
  in
  case msg of
    SetElections result -> onSuccess result <| \elections ->
      ( { model | elections = elections }
      , Cmd.none
      )

    SelectElection electionId ->
      ( { model | selected = Just electionId }
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

    VoteFormOpen ->
      ( { model
        | voteFormOpen = True
        , voteForm = case model.selected of
            Nothing -> model.voteForm
            Just electionId ->
              case lookup (\obj -> obj.electionId == electionId) model.elections of
                Nothing -> model.voteForm
                Just electionInfo -> VoteForm.fromElection electionInfo.election

        }
      , Cmd.none
      )

    VoteFormClose ->
      ( { model
        | voteFormOpen = False
        , voteForm = VoteForm.initial
        }
      , Cmd.none
      )

    ElectionFormMsg electionMsg ->
      let
        config =
          { request = API.createElection info.id
          , finish = \_ -> reload
          , pageInfo = pageInfo
          , zone = zone
          , now = model.now
          }
        (electionForm, cmd) = ElectionsForm.updateWith config electionMsg model.electionForm
      in
        ( { model
          | electionForm = electionForm
          , electionFormOpen = not (Form.isFinished electionMsg)
          }
        , Cmd.map ElectionFormMsg cmd
        )

    SetInitialTime val ->
      ( { model | now = val }
      , Cmd.none
      )

    VoteFormMsg voteMsg ->
      case model.selected of
        Nothing -> (model, Cmd.none)
        Just electionId ->
          case lookup (\obj -> obj.electionId == electionId) model.elections of
            Nothing -> (model, Cmd.none)
            Just electionInfo ->
              let
                config =
                  { request = API.addVote electionId
                  , finish = \_ -> reload
                  , pageInfo = pageInfo
                  , election = electionInfo.election
                  }
                (voteForm, cmd) = VoteForm.updateWith config voteMsg model.voteForm
              in
                ( { model
                  | voteForm = voteForm
                  , voteFormOpen = not (Form.isFinished voteMsg)
                  }
                , Cmd.map VoteFormMsg cmd
                )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Time.Zone -> UnitInfo -> Model -> List (Html Msg)
view zone unit model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Column.view "Elections"
            ( if unit.isAdmin
              then [smallButton "Create Election" ElectionFormOpen]
              else []
            ) <|
            viewElections zone model.elections
          ]
      , case model.selected of
          Nothing -> Html.div [] []
          Just electionId ->
            case lookup (\obj -> obj.electionId == electionId) model.elections of
              Nothing -> Html.div [] []
              Just electionInfo ->
                Html.div
                  [ Html.Attributes.class "column is-half" ]
                  [ Column.view electionInfo.election.title
                      [ case electionInfo.election.resultComputedAt of
                          Nothing ->
                            case electionInfo.votedAt of
                              Just time -> Html.text (viewPosixAt zone time)
                              Nothing -> smallButton "Vote" VoteFormOpen
                          Just _ ->
                            Html.text <|
                            case electionInfo.election.result of
                              Nothing -> "The election has no result."
                              Just result -> "Winner: " ++ result
                      ]
                      [ Html.em
                          []
                          [ Html.text <|
                              let
                                time = viewPosixAt zone electionInfo.election.endingAt
                              in
                                case electionInfo.election.resultComputedAt of
                                  Just _ -> "Closed at " ++ time
                                  Nothing -> "Open until " ++ time
                          ]
                      , Html.div
                          [ Html.Attributes.style "margin-top" "10px"
                          , Html.Attributes.style "margin-bottom" "10px"
                          ]
                          [ Html.strong [] [Html.text "Description"] ]

                      , Html.text electionInfo.election.description

                      , Html.div
                          [ Html.Attributes.style "margin-top" "10px"
                          , Html.Attributes.style "margin-bottom" "10px"
                          ]
                          [ Html.strong [] [Html.text "Choices"] ]

                      , Html.ul
                         [ Html.Attributes.style "margin-top" "0px"
                         ] <|
                         List.map (\choice -> Html.li [] [Html.text choice]) electionInfo.election.choices
                      ]
                  ]
      ]

  , Modal.view model.electionFormOpen ElectionFormClose <|
      Html.map ElectionFormMsg <|
      Form.viewWith "Create Election" (ElectionsForm.view zone) model.electionForm

  , Modal.view model.voteFormOpen VoteFormClose <|
      case model.selected of
        Nothing -> Html.div [] []
        Just electionId ->
          case lookup (\obj -> obj.electionId == electionId) model.elections of
            Nothing -> Html.div [] []
            Just electionInfo ->
              Html.map VoteFormMsg <|
              Form.viewWith "Create Vote" (VoteForm.view electionInfo.election) model.voteForm

  ] ++
  Notification.view model.notification

viewElections : Time.Zone -> List ElectionInfo -> List (Html Msg)
viewElections zone =
  List.map (BoxElection.view zone SelectElection) <<
  List.reverse <<
  List.sortBy (\e -> Time.posixToMillis e.election.endingAt)

smallButton : String -> Msg -> Html Msg
smallButton text msg =
  Html.button
    [ Html.Attributes.class "button is-success is-small"
    , Html.Events.onClick msg
    ]
    [ Html.text text
    ]
