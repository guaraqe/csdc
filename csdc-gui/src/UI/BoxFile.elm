module UI.BoxFile exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (FileUI, filePath)

import Html exposing (Html)
import Html.Attributes

view : FileUI -> Html msg
view file =
  let
    contents =
      [ Html.a
          [ Html.Attributes.href <| filePath file.path
          , Html.Attributes.download file.name
          ]
          [ Html.strong [] [ Html.text file.name ]
          ]
      , Html.br [] []
      , Html.div
          [ Html.Attributes.style "width" "100%"
          , Html.Attributes.style "display" "flex"
          , Html.Attributes.style "justify-content" "space-between"
          ]
          [ Html.div [] [Html.text <| "Size: " ++ String.fromInt file.size]
          , case file.ipfs of
              Nothing -> Html.div [] []
              Just ipfs ->
                Html.div []
                  [ Html.a
                      [ Html.Attributes.href <| "https://ipfs.io/ipfs/" ++ ipfs
                      , Html.Attributes.download file.name
                      ]
                      [ Html.strong [] [ Html.text "IPFS Link" ]
                      ]
                  ]
          ]

      ]
  in
    BoxItem.view
      { id = "thread"
      , onClick = Nothing
      , size = BoxItem.Big
      , selected = False
      , contents = contents
      }
