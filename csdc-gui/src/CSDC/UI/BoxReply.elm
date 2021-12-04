module CSDC.UI.BoxReply exposing
  ( view
  )

import CSDC.UI.BoxItem as BoxItem
import CSDC.Types exposing (Id, Reply, ReplyInfo, MessageType (..))

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List

view : Bool -> ReplyInfo a -> Html (Id (Reply a))
view selected reply =
  let
    contents =
      [ Html.strong []
          [ Html.text <| case reply.mtype of
              Invitation -> "Reply from " ++ reply.message.left
              Submission -> "Reply from " ++ reply.message.right
          ]
      ]
  in
    BoxItem.view
      { id = "repliess"
      , onClick = Just reply.id
      , size = BoxItem.Small
      , selected = selected
      , contents = contents
      }
