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
import Form.UnitElections as ElectionsForm
import Notification exposing (Notification)
import Page as Page exposing (UnitTab (..))
import Types exposing (..)

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Time
import Form

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { elections : List ElectionInfo
  , electionFormOpen : Bool
  , voteFormOpen : Bool
  , selected : Maybe (Id Election)
  , notification : Notification
  , electionForm : ElectionsForm.Model
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
  | SelectElection (Id Election)
  | ElectionFormOpen
  | ElectionFormClose
  | VoteFormOpen
  | VoteFormClose
  | ResetNotification
  | ElectionFormMsg (ElectionsForm.Msg ())

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
      ( { model | voteFormOpen = True }
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

    ElectionFormMsg electionMsg ->
      let
        config =
          { request = API.updateUnit info.id
          , finish = \_ -> onSuccess
          , pageInfo = pageInfo
          }
        (electionForm, cmd) = ElectionsForm.updateWith config electionMsg model.electionForm
      in
        ( { model
          | electionForm = electionForm
          , electionFormOpen = not (Form.isFinished electionMsg)
          }
        , Cmd.map ElectionFormMsg cmd
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
              then [smallButton "Create Election" ElectionFormOpen]
              else []
            ) <|
            viewElections model.elections
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
                      [ case electionInfo.votedAt of
                          Just time -> Html.text (viewPosixAt Time.utc time)
                          Nothing ->
                            case electionInfo.election.resultComputedAt of
                              Nothing ->
                                smallButton "Vote" ElectionFormOpen
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
                                time = viewPosixAt Time.utc electionInfo.election.endingAt
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
      Form.viewWith "Edit Profile" ElectionsForm.view model.electionForm

  , Modal.view model.voteFormOpen VoteFormClose <|
      Html.text "Vote form here."

  ] ++
  Notification.view model.notification

viewElections : List ElectionInfo -> List (Html Msg)
viewElections = List.map (BoxElection.view Time.utc SelectElection)

smallButton : String -> Msg -> Html Msg
smallButton text msg =
  Html.button
    [ Html.Attributes.class "button is-success is-small"
    , Html.Events.onClick msg
    ]
    [ Html.text text
    ]
