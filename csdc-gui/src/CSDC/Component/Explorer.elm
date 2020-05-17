module CSDC.Component.Explorer exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Component.PreviewUnit as PreviewUnit
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Dict
import List
import Element exposing (..)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { left : Panel.Model (Id Unit)
  , center : Panel.Model (Id Unit)
  , right : Panel.Model (Id Unit)
  , notification : Notification
  , preview : Maybe (WithId Unit)
  }

initial : () -> (Model, Cmd Msg)
initial _ =
  ( { left = Panel.initial "Parents"
    , center = Panel.initial "Units"
    , right = Panel.initial "Children"
    , notification = Notification.Empty
    , preview = Nothing
    }
  , Cmd.map (APIMsg Root) <| API.rootUnit
  )

--------------------------------------------------------------------------------
-- Update

type Component
  = Left
  | Center
  | Right
  | Root

type Msg
  = LeftMsg (Panel.Msg (Id Unit))
  | CenterMsg (Panel.Msg (Id Unit))
  | RightMsg (Panel.Msg (Id Unit))
  | APIMsg Component API.Msg
  | ViewUnit (Id Unit)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CenterMsg m ->
      ( { model | center = Panel.update m model.center }
      , case m of
          Panel.SetSelected (Just id) ->
            Cmd.map (APIMsg Center) (API.selectUnit id)
          _ ->
            Cmd.none
      )

    LeftMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | left =
                Panel.update
                  (Panel.SetItems []) model.left

            , center =
                Panel.update
                  (Panel.SetItems <| Panel.getItems model.left)
                  model.center

            , right =
                Panel.update
                  (Panel.SetItems <| Panel.getItems model.center)
                  model.right
            }
          , Cmd.map (APIMsg Left) (API.selectUnit id)
          )

        _ ->
          ( model
          , Cmd.none
          )

    RightMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | left =
                Panel.update
                  (Panel.SetItems <| Panel.getItems model.center)
                  model.left
            , center =
                Panel.update
                  (Panel.SetItems <| Panel.getItems model.right)
                  model.center
            , right =
                Panel.update
                  (Panel.SetItems []) model.right
            }
          , Cmd.map (APIMsg Right) (API.selectUnit id)
          )

        _ ->
          ( model
          , Cmd.none
          )

    ViewUnit m ->
      ( model
      , Cmd.none
      )

    -- XXX: use component
    APIMsg component m ->
      case m of
        API.RootUnit res ->
          case res of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok id ->
               ( model
               , Cmd.map (APIMsg Root) <| API.selectUnit id
               )

        API.SelectUnit id res ->
          case res of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok unit ->
              case component of
                Root ->
                  let
                    dict = [(id,unit.name)]
                    center = Panel.update (Panel.SetItems dict) model.center
                  in
                    ( { model | center = center }
                    , Cmd.none
                    )

                -- When the message comes from the center:
                --
                -- - change selected
                -- - display preview
                -- - launch commands for parents and children
                --
                Center ->
                  let
                    center =
                      Panel.update (Panel.SetSelected (Just id)) model.center

                    preview =
                      Just { id = id, value = unit }
                  in
                    ( { model | center = center, preview = preview }
                    , Cmd.map (APIMsg Center) <|
                      Cmd.batch
                        [ API.getUnitChildren id
                        , API.getUnitParents id
                        ]
                    )

                Left ->
                  let
                    center =
                      Panel.update (Panel.SetSelected (Just id)) model.center

                    preview =
                      Just { id = id, value = unit }
                  in
                    ( { model | center = center, preview = preview }
                    , Cmd.map (APIMsg Center) <| API.getUnitParents id
                    )

                Right ->
                  let
                    center =
                      Panel.update (Panel.SetSelected (Just id)) model.center

                    preview =
                      Just { id = id, value = unit }
                  in
                    ( { model | center = center, preview = preview }
                    , Cmd.map (APIMsg Center) <| API.getUnitChildren id
                    )


        API.GetUnitChildren res ->
          case res of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok units ->
              let
                pairs = toPairs units
                right = Panel.update (Panel.SetItems pairs) model.right
              in
                ( { model | right = right }
                , Cmd.none
                )

        API.GetUnitParents res ->
          case res of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok units ->
              let
                pairs = toPairs units
                left = Panel.update (Panel.SetItems pairs) model.left
              in
                ( { model | left = left }
                , Cmd.none
                )

        _ ->
          ( model
          , Cmd.none
          )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  [ row
      [ height <| fillPortion 3
      , width fill
      , spacing 10
      ]
      [ map LeftMsg <| Panel.view model.left
      , map CenterMsg <| Panel.view model.center
      , map RightMsg <| Panel.view model.right
      ]
  , row
      [ height <| fillPortion 1
      , width fill
      ] <|
      case model.preview of
        Nothing ->
          []
        Just unit ->
          PreviewUnit.view unit.value (ViewUnit unit.id)
  ] ++
  Notification.view model.notification

--------------------------------------------------------------------------------
-- Helpers

toPairs : IdMap a (WithId Unit) -> List (Id Unit, String)
toPairs =
  let
    toPair (_, withId) = (withId.id, withId.value.name)
  in
    List.map toPair << idMapToList
