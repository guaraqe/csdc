module UI.PreviewImageText exposing
  ( view
  )

import UI.Preview as Preview
import Types exposing (..)
import Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Markdown

view : { value | name : String, description : String, image : FilePath } -> msg -> Html msg
view value msg = Preview.make
  [ Html.div
     [ Html.Attributes.style "width" "100%"
     , Html.Attributes.style "min-height" "156px"
     ]
     [ Html.h4 [] [ Html.text value.name ]

     , Html.figure
         [ Html.Attributes.class "image is-96x96"
         , Html.Attributes.style "float" "left"
         , Html.Attributes.style "margin" "5px 30px 10px 0px"
         ]
         [ Html.img
             [ Html.Attributes.src <| filePath value.image
             , Html.Attributes.style "border-radius" "10%"
             , Html.Attributes.alt "Profile photo"
             ]
             []
         ]

     , Markdown.toHtml
         [ Html.Attributes.class "content"
         , Html.Attributes.style "list-style-position" "inside"
         ]
         value.description
     ]

  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick msg
      ]
      [ Html.text "See profile"
      ]
  ]
