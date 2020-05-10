module CSDC.Component.Admin exposing
  ( Model
  , initial
  , Msg
  , update
  , view
  )

import CSDC.Component.Admin.NewMember as NewMember
import CSDC.Component.Admin.NewPerson as NewPerson
import CSDC.Component.Admin.NewUnit as NewUnit
import CSDC.Component.Admin.NewSubpart as NewSubpart

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import List
import Maybe
import Maybe exposing (withDefault)
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { newMember : NewMember.Model
  , newPerson : NewPerson.Model
  , newSubpart : NewSubpart.Model
  , newUnit : NewUnit.Model
  }

initial : Model
initial =
  { newMember = NewMember.initial
  , newPerson = NewPerson.initial
  , newSubpart = NewSubpart.initial
  , newUnit = NewUnit.initial
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = NewMemberMsg NewMember.Msg
  | NewPersonMsg NewPerson.Msg
  | NewSubpartMsg NewSubpart.Msg
  | NewUnitMsg NewUnit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewUnitMsg m ->
      let
        (newUnit, cmd) = NewUnit.update m model.newUnit
      in
        ( { model | newUnit = newUnit }
        , Cmd.map NewUnitMsg cmd
        )

    NewPersonMsg m ->
      let
        (newPerson, cmd) = NewPerson.update m model.newPerson
      in
        ( { model | newPerson = newPerson }
        , Cmd.map NewPersonMsg cmd
        )

    NewMemberMsg m ->
      let
        (newMember, cmd) = NewMember.update m model.newMember
      in
        ( { model | newMember = newMember }
        , Cmd.map NewMemberMsg cmd
        )

    NewSubpartMsg m ->
      let
        (newSubpart, cmd) = NewSubpart.update m model.newSubpart
      in
        ( { model | newSubpart = newSubpart }
        , Cmd.map NewSubpartMsg cmd
        )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  [ Element.map NewPersonMsg <| NewPerson.view model.newPerson
  , Element.map NewUnitMsg <| NewUnit.view model.newUnit
  , Element.map NewMemberMsg <| NewMember.view model.newMember
  , Element.map NewSubpartMsg <| NewSubpart.view model.newSubpart
  ]
