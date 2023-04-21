module UI.BoxElection exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (Id, Election, ElectionInfo)

import Html exposing (Html)
import Html.Attributes
import Html.Events

view : (Id Election -> msg) -> ElectionInfo -> Html msg
view toMsg electionInfo =
  let
    contents =
      [ Html.div [] [ Html.strong [] [ Html.text electionInfo.election.title ] ]
      , Html.div [] [ Html.text electionInfo.election.description ]
      , case electionInfo.votedAt of
          Just date ->
              Html.text "Voted"
          Nothing ->
            Html.button
              [ Html.Attributes.class "button is-success is-small"
              , Html.Events.onClick (toMsg electionInfo.electionId)
              ]
              [ Html.text "Vote"
              ]
      ]
  in
    BoxItem.view
      { id = "election"
      , onClick = Nothing
      , size = BoxItem.Big
      , selected = False
      , contents = contents
      }
