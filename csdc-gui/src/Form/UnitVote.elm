module Form.UnitVote exposing
  ( Model
  , initial
  , fromElection
  , Msg
  , updateWith
  , view
  )

import API as API
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)
import Form
import Page

import Dict exposing (Dict)
import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { simpleMajority : Field (Maybe String) String
  , majorityConsensus : Dict ElectionChoice (Field (Maybe Grade) (Maybe Grade))
  , notification : Notification
  }

initial : Model
initial =
  { simpleMajority = Field.required "Choice"
  , majorityConsensus = Dict.empty
  , notification = Notification.Empty
  }

fromElection : Election -> Model
fromElection election =
  { initial
  | majorityConsensus =
      case election.electionType of
        MajorityConsensus ->
          Dict.fromList <|
          List.map (\choice -> (choice, Field.optional "")) election.choices
        _ -> Dict.empty
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | simpleMajority = Field.reload model.simpleMajority
  , majorityConsensus = Dict.map (\_ -> Field.reload) model.majorityConsensus
  }

parse : Election -> Model -> Maybe VotePayload
parse election model =
  Result.toMaybe <|
  case election.electionType of
    SimpleMajority ->
      Field.with model.simpleMajority <| \choice ->
      Ok (VotePayloadSimpleMajority (Just choice))

    MajorityConsensus ->
      case Field.withDict model.majorityConsensus of
        Err errs -> Err errs
        Ok grades ->
          let
            f (k,m) = case m of
              Nothing -> Nothing
              Just x -> Just (k,x)
          in
            Ok <|
            VotePayloadMajorityConsensus <|
            Dict.fromList <| List.filterMap f <| Dict.toList grades

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { pageInfo : Page.Info
  , request : VotePayload -> Cmd (API.Response a)
  , election : Election
  , finish : a -> Cmd (Msg a)
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse config.election
  , request = config.request
  , finish = config.finish
  }

type alias Msg a = Form.Msg ModelMsg () a

type ModelMsg
  = SetSimpleMajority ElectionChoice
  | SetMajorityConsensus ElectionChoice Grade

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetSimpleMajority val ->
      ( { model | simpleMajority = Field.set (Just val) model.simpleMajority }
      , Cmd.none
      )
    SetMajorityConsensus choice grade ->
      ( { model
        | majorityConsensus =
            let
              updateField mfield =
                case mfield of
                  Nothing -> Nothing
                  Just field -> Just <| Field.set (Just grade) field
            in
              Dict.update choice updateField model.majorityConsensus
         }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Election -> Model -> List (Html (Msg a))
view election model =
  [ case election.electionType of
      SimpleMajority ->
        Input.radio model.simpleMajority SetSimpleMajority <|
          List.map (\choice ->
            { name = choice, value = choice }
          ) election.choices

      MajorityConsensus ->
        Input.grades model.majorityConsensus SetMajorityConsensus

  , Input.button "Save" ()
  ]
