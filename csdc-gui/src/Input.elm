module Input exposing (..)

import Field exposing (Field (..), Status (..))
import Form
import Types exposing (ElectionChoice, Grade (..), viewPosixAt)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import SingleDatePicker exposing (DatePicker)
import Time exposing (Posix)

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

password : Field String a -> (String -> msg) -> Html (Form.Msg msg r b)
password field makeMsg =
  wrapper field <|
    Html.input
      [ case Field.status field of
          Invalid _ -> Html.Attributes.class "input is-danger"
          _ -> Html.Attributes.class "input"
      , Html.Attributes.type_ "password"
      , Html.Attributes.placeholder (Field.name field)
      , Html.Attributes.value (Field.raw field)
      , Html.Events.onInput (Form.ModelMsg << makeMsg)
      ]
      []

email : Field String a -> (String -> msg) -> Html (Form.Msg msg r b)
email field makeMsg =
  wrapper field <|
    Html.input
      [ case Field.status field of
          Invalid _ -> Html.Attributes.class "input is-danger"
          _ -> Html.Attributes.class "input"
      , Html.Attributes.type_ "email"
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

dateAndTime :
  Field (Maybe Posix) Posix ->
  Time.Zone ->
  SingleDatePicker.Settings (Form.Msg msg r b) ->
  DatePicker ->
  msg ->
  Html (Form.Msg msg r b)
dateAndTime field zone settings datePicker openMsg =
  wrapper field <|
    Html.div [] <|
      [ Html.button
          [ Html.Attributes.class "button is-light is-small"
          , Html.Attributes.style "margin-right" "5px"
          , Html.Events.onClick (Form.ModelMsg openMsg)
          ]
          [ Html.text "Select"
          ]
      , case Field.status field of
          Valid posix -> Html.text (viewPosixAt zone posix)
          _ -> Html.text "Select a date."
      , Html.div
          [ Html.Attributes.style "z-index" "999" ]
          [ SingleDatePicker.view settings datePicker
          ]
      ]


checkbox : Field Bool a -> (Bool -> msg) -> String -> Html (Form.Msg msg r b)
checkbox field makeMsg checkboxText =
  wrapper field <|
    Html.label
      [ Html.Attributes.class "checkbox" ]
      [ Html.input
          [ case Field.status field of
              Invalid _ -> Html.Attributes.class "checkbox is-danger"
              _ -> Html.Attributes.class "checkbox"
          , Html.Attributes.type_ "checkbox"
          , Html.Attributes.checked (Field.raw field)
          , Html.Events.onCheck (Form.ModelMsg << makeMsg)
          ]
          []
      , Html.text checkboxText
      ]

radio :
  Field (Maybe a) x ->
  (a -> msg) ->
  List { name : String, value : a } ->
  Html (Form.Msg msg r b)
radio field makeMsg choices =
  wrapper field <|
    Html.div [] <| List.map (\{ name, value } ->
      Html.label
        [ Html.Attributes.class "radio" ]
        [ Html.input
            [ case Field.status field of
                Invalid _ -> Html.Attributes.class "radio is-danger"
                _ -> Html.Attributes.class "radio"
            , Html.Attributes.type_ "radio"
            , Html.Attributes.checked (Field.raw field == Just value)
            , Html.Events.onCheck (\_ -> Form.ModelMsg <| makeMsg value)
            ]
            []
        , Html.text name
        ]
      ) choices

grades :
  Dict ElectionChoice (Field (Maybe Grade) (Maybe Grade)) ->
  (ElectionChoice -> Grade -> msg) ->
  Html (Form.Msg msg r b)
grades fields makeMsg =
    Html.table
      [ Html.Attributes.class "table is-narrow" ]
      [ Html.thead []
          [ Html.tr
              [ Html.Attributes.style "white-space" "nowrap"
              ]
              [ Html.th [] [ Html.text "Candidates" ]
              , Html.th [] [ Html.text "Reject" ]
              , Html.th [] [ Html.text "Poor" ]
              , Html.th [] [ Html.text "Acceptable" ]
              , Html.th [] [ Html.text "Good" ]
              , Html.th [] [ Html.text "Very Good" ]
              , Html.th [] [ Html.text "Excellent" ]
              ]
          ]
      , Html.tbody [] <|
          List.map (\(choice, field) ->
            Html.tr [] <|
              [ Html.th [] [ Html.text choice ] ] ++
              List.map (\grade ->
                Html.td
                  [ Html.Attributes.style "text-align" "center"
                  ]
                  [ Html.input
                      [ case Field.status field of
                          Invalid _ -> Html.Attributes.class "radio is-danger"
                          _ -> Html.Attributes.class "radio"
                      , Html.Attributes.type_ "radio"
                      , Html.Attributes.checked (Field.raw field == Just grade)
                      , Html.Events.onCheck (\_ -> Form.ModelMsg <| makeMsg choice grade)
                      ]
                      []
                  ]
              ) [ GradeVeryBad
                , GradeBad
                , GradeAcceptable
                , GradeGood
                , GradeVeryGood
                , GradeExcellent
                ]
          ) (Dict.toList fields)
      ]

type alias TextFieldModel a = Field (Dict Int (Field String a)) (List a)

type TextListAction
  = TextListAdd
  | TextListUpdate Int String
  | TextListRemove Int

textListUpdate :
  Field String a -> TextListAction -> TextFieldModel a -> TextFieldModel a
textListUpdate newField action (Field field) =
  let
    newRaw =
      case action of
        TextListAdd ->
          let
            newIndex =
              case List.maximum (Dict.keys field.raw) of
                Nothing -> 0
                Just index -> index + 1
          in
            Dict.insert newIndex newField field.raw

        TextListUpdate index string ->
          let
            update m =
              case m of
                Nothing -> Nothing
                Just f -> Just <| Field.set string f
          in
            Dict.update index update field.raw

        TextListRemove index ->
          Dict.remove index field.raw
  in
    Field { field | raw = newRaw }

textList : TextFieldModel a -> (TextListAction -> msg) -> Html (Form.Msg msg r b)
textList fields toMsg =
  wrapper fields <|
    Html.div [] <|
      ( List.map (\(index, field) ->
          Html.div
            [ Html.Attributes.class "columns  is-vcentered" ]
            [ Html.div
                [ Html.Attributes.class "column" ]
                [ text field (toMsg << TextListUpdate index) ]
            , Html.div
                [ Html.Attributes.class "column is-one-fifth" ]
                [ Html.button
                    [ Html.Attributes.class "button is-light is-small"
                    , Html.Events.onClick (Form.ModelMsg <| toMsg <| TextListRemove index)
                    ]
                    [ Html.text "X"
                    ]
                ]
            ]
        ) (Dict.toList (Field.raw fields))
      ) ++
      [ Html.button
          [ Html.Attributes.class "button is-light is-small"
          , Html.Events.onClick (Form.ModelMsg <| toMsg TextListAdd)
          ]
          [ Html.text "Add"
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
