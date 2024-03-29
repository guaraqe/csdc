module UI.BoxElection exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (Id, Election, ElectionInfo, viewPosixAt)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Time

view : Time.Zone -> (Id Election -> msg) -> ElectionInfo -> Html msg
view zone toMsg electionInfo =
  let
    contents =
      [ Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-three-quarters" ]
              [ Html.div [] [ Html.strong [] [ Html.text electionInfo.election.title ] ]
              , Html.div []
                [ Html.text <|
                    case electionInfo.election.resultComputedAt of
                      Nothing ->
                       "Until " ++ viewPosixAt zone electionInfo.election.endingAt ++ "."
                      Just _ ->
                        case electionInfo.election.result of
                          Nothing -> "The election had no winner."
                          Just choice -> "Winner: " ++ choice ++ "."
                ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-quarter" ]
              [ Html.text <| String.fromInt electionInfo.totalVotes ++ " votes"
              , Html.br [] []
              , Html.em []
                  [ case electionInfo.votedAt of
                      Just date ->
                        Html.text "Voted"
                      Nothing ->
                        Html.text "Vote pending"
                  ]
              ]
          ]
      ]
  in
    BoxItem.view
      { id = "election"
      , onClick = Just (toMsg electionInfo.electionId)
      , size = BoxItem.Big
      , selected = False
      , contents = contents
      }
