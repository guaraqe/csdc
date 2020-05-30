module CSDC.Component.PreviewReply exposing
  ( view
  )

import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border

view : Reply a -> msg -> List (Element msg)
view (Reply msg) event =
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
            [ case msg.rtype of
                Accept -> text "Accepted"
                Reject -> text "Rejected"
            ]
        , column
            [ alignRight ]
            [ button event "TODO" ]
        ]
     , row
        [ height fill
        , padding 10
        ]
        [ text msg.text
        ]
     ]
  ]
