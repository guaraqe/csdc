module Input exposing
  ( text
  , radio
  )

import Field exposing (Field)

import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input

text :
  { onChange : String -> msg
  , field : Field String a
  } ->
  Element msg
text opts =
  Element.column
    [ Element.spacing 10 ]
    [ Input.text
        []
        { onChange = opts.onChange
        , placeholder = Nothing
        , label = Input.labelAbove [] (Element.text (Field.name opts.field))
        , text = Field.raw opts.field
        }
    , Element.column
        [ Font.color (Element.rgb 1 0 0)
        ]
        (List.map Element.text (Field.errors opts.field))
    ]


radio :
  { onChange : option -> msg
  , options : List (option, String)
  , field : Field (Maybe option) option
  } ->
  Element msg
radio opts =
  Input.radioRow
    [ Element.padding 10
    , Element.spacing 20
    ]
    { onChange = opts.onChange
    , selected = Field.raw opts.field
    , label = Input.labelAbove [] (Element.text (Field.name opts.field))
    , options = List.map (\(a,b) -> Input.option a (Element.text b)) opts.options
    }

