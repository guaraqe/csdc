module CSDC.Form.Post exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import CSDC.API as API
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import Field exposing (Field)
import Validation
import Form

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { text : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { text = Field.requiredString "Text"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | text = Field.reload model.text
  }

parse : Model -> Maybe NewPost
parse model =
  let
    result =
      Validation.andThen (Field.validate model.text) <| \text ->
      Validation.valid
        { text = text
        }
  in
    case Validation.validate result of
      Err _ -> Nothing
      Ok unit -> Just unit

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { id : Id Thread
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.createThreadPost config.id
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetText String

type alias Msg = Form.Msg ModelMsg () (Id Post)

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetText val ->
      ( { model | text = Field.set val model.text }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.textarea model.text SetText
  , Input.button "Create" ()
  ]