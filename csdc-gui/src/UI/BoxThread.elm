module UI.BoxThread exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (Id, Thread, ThreadInfo, viewPosixAt)

import Html exposing (Html)
import Html.Attributes
import Time

view : Time.Zone -> Bool -> ThreadInfo -> Html (Id Thread)
view zone selected thread =
  let
    contents =
      [ Html.div
          [ Html.Attributes.class "is-flex is-justify-content-space-between"
          ]
          [ Html.strong [] [ Html.text thread.subject ]
          , Html.text <| viewPosixAt zone thread.createdAt
          ]
      , Html.div
          [ Html.Attributes.class "is-flex is-justify-content-space-between"
          ]
          [ Html.em [] [ Html.text <| "Author: " ++ thread.authorName ]
          , Html.text <| String.fromInt thread.messages ++ " posts"
          ]
      ]
  in
    BoxItem.view
      { id = "thread"
      , onClick = Just thread.id
      , size = BoxItem.Big
      , selected = selected
      , contents = contents
      }
