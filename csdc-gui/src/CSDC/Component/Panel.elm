module CSDC.Component.Panel exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  , getItems
  , getSelected
  )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Dict
import Dict exposing (Dict)
import List

--------------------------------------------------------------------------------
-- Model

type Model i = Model
  { name : String
  , items : List (i, String)
  , selected : Maybe i
  }

initial : String -> Model i
initial name = Model
  { name = name
  , items = []
  , selected = Nothing
  }

getSelected : Model i -> Maybe i
getSelected (Model m) = m.selected

getItems : Model i -> List (i, String)
getItems (Model m) = m.items

--------------------------------------------------------------------------------
-- Update

type Msg i
  = SetItems (List (i, String))
  | SetSelected (Maybe i)

update : Msg i -> Model i -> Model i
update msg (Model model) =
  case msg of
    SetItems items ->
      Model { model | items = items }

    SetSelected selected ->
      Model { model | selected = selected }

--------------------------------------------------------------------------------
-- View

view : Model i -> Element (Msg i)
view (Model model) =
  column
    [ height fill
    , width <| fillPortion 1
    , Border.width 1
    , Border.color <| rgb255 92 99 118
    , Border.rounded 5
    ]
    ( [ viewTitle model.name ] ++
      List.map (viewItem model.selected) model.items
    )

viewTitle : String -> Element (Msg i)
viewTitle name =
  row
    [ height (px 50)
    , padding 10
    , width fill
    , Background.color <| rgb255 92 99 118
    , Font.size 24
    ]
    [ el [ centerX ] (text name)
    ]

viewItem : Maybe i -> (i, String) -> Element (Msg i)
viewItem selected (this,name) =
  row
    [ height (px 30)
    , padding 10
    , width fill
    , Events.onClick <| SetSelected (Just this)
    , if selected == Just this
      then Background.color <| rgb255 142 151 164
      else Background.color <| rgb255 255 255 255
    ]
    [ text name
    ]
