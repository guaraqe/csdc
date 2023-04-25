module Form.UnitElections exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import API as API

import Form
import Page
import Html exposing (Html)
import Time exposing ( millisToPosix)
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)
import Debug










--------------------------------------------------------------------------------
-- Model

type alias Model =
  { title : Field String String
  , description : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { title = Field.requiredString "Title"
  , description = Field.requiredString "Description"
  , notification = Notification.Empty
  }



reload : Model -> Model
reload model =
  { model
  | title = Field.reload model.title
  , description = Field.reload model.description
  }

-- Also unit update...
parse : Model -> Maybe NewElection
parse model = Result.toMaybe <|
  Field.with model.title <| \title ->
  Field.with model.description <| \description ->
 
  Ok
    { title = title
    , description = description
    , unitId = Debug.todo "LALA"
    , choices = ["Choice 1", "Choice 2"]
    , electionType = ["Choice 1", "Choice 2"]
    , visibleVotes = ["true"]
    , endingAt = ["1"]
    
    }
  

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { pageInfo : Page.Info
  , request : NewUnit -> Cmd (API.Response a)
  , finish : a -> Cmd (Msg a)
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = config.request
  , finish = config.finish
  }

type alias Msg a = Form.Msg ModelMsg () a

type ModelMsg
  = SetTitle String
  | SetDescription String

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetTitle val ->
      ( { model | title = Field.set val model.title }
      , Cmd.none
      )
    SetDescription val ->
      ( { model | description = Field.set val model.description }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html (Msg a))
view model =
  [ Input.text model.title SetTitle
  , Input.textarea model.description SetDescription
  , Input.button "Save" ()
  ]
