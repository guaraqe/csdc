module CSDC.Input exposing
  ( button
  , EditableMode (..)
  , EditableMsg (..)
  , editable
  )

import Element exposing (Element)

import Element as Element
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border

--------------------------------------------------------------------------------
-- Button

button : msg -> String -> Element msg
button msg txt =
  Input.button
    [ Background.color <| Element.rgb255 142 151 164
    , Font.color <| Element.rgb255 243 243 244
    , Element.paddingXY 20 10
    ]
    { onPress = Just msg
    , label = Element.text txt
    }

--------------------------------------------------------------------------------
-- Editable

type EditableMode
  = EditableModeEdit
  | EditableModeShow

type EditableMsg
  = EditableEdit
  | EditableSave
  | EditableUpdate String

editable :
  { canEdit : Bool
  , mode : EditableMode
  , label : String
  , value : String
  , event : EditableMsg -> msg
  } -> Element msg
editable options =
  Element.row [ Element.spacing 10 ] <|
    case options.canEdit of
      False ->
        [ Element.text options.value
        ]
      True ->
        case options.mode of
          EditableModeShow ->
            [ Element.column []
                [ Element.text options.value
                ]
            , Element.column []
                [ Input.button
                  [ Font.color <| Element.rgb255 142 151 164
                  ]
                  { onPress = Just <| options.event <| EditableEdit
                  , label = Element.text "Edit"
                  }
                ]
            ]

          EditableModeEdit ->
            [ Element.column
                [ Element.width <| Element.fillPortion 1
                ]
                [ Input.text []
                  { onChange = options.event << EditableUpdate
                  , text = options.value
                  , placeholder = Nothing
                  , label = Input.labelAbove [] (Element.text options.label)
                  }
                ]
            , Element.column []
                [ Input.button
                  [ Font.color <| Element.rgb255 142 151 164
                  ]
                  { onPress = Just <| options.event <| EditableSave
                  , label = Element.text "Save"
                  }
                ]
            ]
