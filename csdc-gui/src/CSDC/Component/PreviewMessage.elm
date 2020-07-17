module CSDC.Component.PreviewMessage exposing
  ( view
  )

import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border

view : MessageInfo a -> (MessageType -> ReplyType -> msg) -> List (Element msg)
view (MessageInfo msg) event =
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
            [ case msg.mtype of
                Invitation -> text <| "Invitation from " ++ msg.right
                Submission -> text <| "Submission from " ++ msg.left
            ]
        , column
            [ alignRight ]
            [ button (event msg.mtype Accept) "Accept" ]
        , column
            [ alignRight ]
            [ button (event msg.mtype Reject) "Reject" ]
        ]
     , row
        [ height fill
        , padding 10
        ]
        [ text msg.text
        ]
     ]
  ]
