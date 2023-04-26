module Page.UnitForum exposing
  ( Model
  , setup
  , setupThread
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxPost as BoxPost
import UI.BoxThread as BoxThread
import UI.Modal as Modal
import UI.Column as Column
import Notification exposing (Notification)
import Page as Page exposing (UnitTab (..))
import Types exposing (..)
import Form.Thread as ThreadForm
import Form.Post as PostForm
import Form

import Browser.Dom as Dom
import Delay
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import Time

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { threads : List ThreadInfo
  , posts : List PostInfo
  , threadForm : ThreadForm.Model
  , threadFormOpen : Bool
  , postForm : PostForm.Model
  , postFormOpen : Bool
  , notification : Notification
  }

initial : Model
initial =
  { threads = []
  , posts = []
  , threadForm = ThreadForm.initial
  , threadFormOpen = False
  , postForm = PostForm.initial
  , postFormOpen = False
  , notification = Notification.Empty
  }

setup : Id Unit -> Maybe (Id Thread) -> Cmd Msg
setup id mtid = Cmd.batch
  [ Cmd.map SetThreads <| API.getUnitThreads id
  , setupThread mtid
  ]

setupThread : Maybe (Id Thread) -> Cmd Msg
setupThread mtid =
  case mtid of
    Nothing -> Cmd.none
    Just tid -> Cmd.map SetPosts <| API.getThreadPosts tid

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetThreads (API.Response (List ThreadInfo))
  | SetPosts (API.Response (List PostInfo))
  | SelectThread (Id Thread)
  | ThreadFormMsg ThreadForm.Msg
  | ThreadFormOpen
  | ThreadFormClose
  | PostFormMsg PostForm.Msg
  | PostFormOpen
  | PostFormClose
  | Focus
  | FocusResult (Result Dom.Error ())
  | ResetNotification

update : UnitInfo -> Maybe (Id Thread) -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info selected pageInfo msg model =
  let
    onSuccess = Notification.withResponse pageInfo ResetNotification model
    reload tid = Page.goTo pageInfo <| Page.Unit (UnitForum tid) info.id
  in
  case msg of
    SetThreads result -> onSuccess result <| \threads ->
      ( { model | threads = threads }
      , Cmd.none
      )

    SetPosts result -> onSuccess result <| \posts ->
      ( { model | posts = posts }
      , Delay.after 0.1 Delay.Second Focus
      )

    SelectThread tid ->
      ( model
      , reload (Just tid)
      )

    ThreadFormOpen ->
      ( { model | threadFormOpen = True }
      , Cmd.none
      )

    ThreadFormClose ->
      ( { model | threadFormOpen = False }
      , Cmd.none
      )

    ThreadFormMsg unitMsg ->
      let
        config =
          { id = info.id
          , finish = \tid -> reload (Just tid)
          , pageInfo = pageInfo
          }
        (threadForm, cmd) = ThreadForm.updateWith config unitMsg model.threadForm
      in
        ( { model
          | threadForm = threadForm
          , threadFormOpen = not (Form.isFinished unitMsg)
          }
        , Cmd.batch
            [ Cmd.map ThreadFormMsg cmd
            , if Form.isFinished unitMsg
              then Cmd.map SetThreads <| API.getUnitThreads info.id
              else Cmd.none
            ]
        )

    PostFormOpen ->
      ( { model | postFormOpen = True }
      , Cmd.none
      )

    PostFormClose ->
      ( { model | postFormOpen = False }
      , Cmd.none
      )

    PostFormMsg unitMsg ->
      case selected of
        Nothing ->
          (model, Cmd.none)
        Just id ->
          let
            config =
              { id = id
              , finish = reload selected
              , pageInfo = pageInfo
              }
            (postForm, cmd) = PostForm.updateWith config unitMsg model.postForm
          in
            ( { model
              | postForm = postForm
              , postFormOpen = not (Form.isFinished unitMsg)
              }
            , Cmd.map PostFormMsg cmd
            )

    Focus ->
      ( model
      , Dom.getElement "Posts-items"
        |> Task.andThen (\p -> Dom.setViewportOf "Posts-items" 0 p.scene.height)
        |> Task.attempt FocusResult
      )

    FocusResult r ->
      ( model
      , case r of
          Err e -> Cmd.none --Debug.log ("Could not focus: " ++ Debug.toString e) Cmd.none
          _ -> Cmd.none
      )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Time.Zone -> UnitInfo -> Maybe (Id Thread) -> Model -> List (Html Msg)
view zone unit selected model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Threads" [smallButton "New Thread" ThreadFormOpen] <|
            viewThreads zone selected model.threads
          ]
      , Html.div
          [ Html.Attributes.class "column is-two-thirds" ]
          [ if List.isEmpty model.posts
            then
              Html.div [] []
            else
              Column.view "Posts" [smallButton "New Post" PostFormOpen] <|
              viewPosts zone model.posts
          ]
      ]

      , Modal.view model.threadFormOpen ThreadFormClose <|
          Html.map ThreadFormMsg <|
          Form.viewWith "New Thread" ThreadForm.view model.threadForm

      , Modal.view model.postFormOpen PostFormClose <|
          Html.map PostFormMsg <|
          Form.viewWith "New Post" PostForm.view model.postForm

  ] ++
  Notification.view model.notification

viewThreads : Time.Zone -> Maybe (Id Thread) -> List ThreadInfo -> List (Html Msg)
viewThreads zone tid threads =
  let
    toBox thread =
      Html.map SelectThread <|
      BoxThread.view zone (tid == Just thread.id) thread
  in
    List.map toBox threads

viewPosts : Time.Zone -> List PostInfo -> List (Html Msg)
viewPosts zone = List.map (BoxPost.view zone)

smallButton : String -> msg -> Html msg
smallButton txt msg =
  Html.button
    [ Html.Attributes.class "button is-success is-small"
    , Html.Events.onClick msg
    ]
    [ Html.text txt
    ]

