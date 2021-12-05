module CSDC.Input exposing (..)

import Field exposing (Field (..), Status (..))
import Form

import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Button

button : String -> r -> Html (Form.Msg msg r b)
button txt r =
  Html.button
    [ Html.Attributes.class "button is-link is-pulled-right"
    , Html.Events.onClick (Form.Submit r)
    ]
    [ Html.text txt
    ]

buttonDanger : String -> r -> Html (Form.Msg msg r b)
buttonDanger txt r =
  Html.button
    [ Html.Attributes.class "button is-danger is-pulled-right"
    , Html.Events.onClick (Form.Submit r)
    ]
    [ Html.text txt
    ]

--------------------------------------------------------------------------------
-- Text input

wrapper : Field a b -> Html msg -> Html msg
wrapper field div =
  Html.div
    [ Html.Attributes.class "field"
    ] <|
    [ Html.label
        [ Html.Attributes.class "label" ]
        [ Html.text (Field.name field) ]
    , Html.div
        [ Html.Attributes.class "control" ]
        [ div ]
    ] ++
      let
        makeError err =
          Html.p
            [ Html.Attributes.class "help is-danger" ]
            [ Html.text err ]
      in
        List.map makeError (Field.errors field)

text : Field String a -> (String -> msg) -> Html (Form.Msg msg r b)
text field makeMsg =
  wrapper field <|
    Html.input
      [ case Field.status field of
          Invalid _ -> Html.Attributes.class "input is-danger"
          _ -> Html.Attributes.class "input"
      , Html.Attributes.type_ "text"
      , Html.Attributes.placeholder (Field.name field)
      , Html.Attributes.value (Field.raw field)
      , Html.Events.onInput (Form.ModelMsg << makeMsg)
      ]
      []


textarea : Field String a -> (String -> msg) -> Html (Form.Msg msg r b)
textarea field makeMsg =
  Html.div []
    [ wrapper field <|
      Html.textarea
        [ case Field.status field of
            Invalid _ -> Html.Attributes.class "textarea is-danger"
            _ -> Html.Attributes.class "textarea"
        , Html.Attributes.placeholder (Field.name field)
        , Html.Attributes.value (Field.raw field)
        , Html.Events.onInput (Form.ModelMsg << makeMsg)
        ]
        []
    , Html.span
        [ Html.Attributes.class "is-size-7" ]
        [ Html.text "This text can be formated with "
        , Html.a
            [ Html.Attributes.href "https://www.markdownguide.org/cheat-sheet/"
            , Html.Attributes.target "_blank"
            ]
            [ Html.text "Markdown" ]
        ]
    ]

--------------------------------------------------------------------------------
-- Text input

select : List (a, String) -> Field (Maybe a) c -> (Maybe a -> msg) -> Html (Form.Msg msg r b)
select items field makeMsg =
  wrapper field <|
    Html.div
      [ Html.Attributes.class "select"
      ]
      [ Html.select [] <|
          List.map (Html.map (Form.ModelMsg << makeMsg) << selectItem) <|
          [(Nothing, "Select")] ++ List.map (\(a,l) -> (Just a,l)) items
      ]

selectItem : (Maybe a, String) -> Html (Maybe a)
selectItem (a, label) =
  Html.option
    [ Html.Events.onClick a ]
    [ Html.text label ]
