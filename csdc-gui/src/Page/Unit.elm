module Page.Unit exposing
  ( Model
  , initial
  , setup
  , changeTab
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.Tabs as Tabs
import Notification exposing (Notification)
import Page as Page
import Types exposing (..)
import Page.UnitInfo as UnitInfo
import Page.UnitAdmin as UnitAdmin
import Page.UnitFiles as UnitFiles
import Page.UnitForum as UnitForum
import WebData exposing (WebData)

import Html exposing (Html)
import Html.Attributes

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { info : WebData UnitInfo
  , unitInfo : UnitInfo.Model
  , unitAdmin : UnitAdmin.Model
  , unitFiles : UnitFiles.Model
  , unitForum : UnitForum.Model
  , notification : Notification
  }

initial : Model
initial =
  { info = WebData.Loading
  , unitInfo = UnitInfo.initial
  , unitAdmin = UnitAdmin.initial
  , unitFiles = UnitFiles.initial
  , unitForum = UnitForum.initial
  , notification = Notification.Empty
  }

setup : Id Unit -> Page.UnitTab -> Cmd Msg
setup id tab =
  Cmd.batch
    [ Cmd.map (GetUnitInfo tab) <| API.getUnitInfo id
    ]

changeTab : Id Unit -> Page.UnitTab -> Page.UnitTab -> Cmd Msg
changeTab id prev tab =
  case (prev, tab) of
    (Page.UnitForum _, Page.UnitForum mtid) ->
      Cmd.map (UnitForumMsg mtid) (UnitForum.setupThread mtid)
    _ ->
      setupTab id tab

setupTab : Id Unit -> Page.UnitTab -> Cmd Msg
setupTab id tab =
  case tab of
    Page.UnitInfo -> setup id Page.UnitInfo
    Page.UnitAdmin -> Cmd.map UnitAdminMsg <| UnitAdmin.setup id
    Page.UnitFiles -> Cmd.map UnitFilesMsg <| UnitFiles.setup id
    Page.UnitForum mtid -> Cmd.map (UnitForumMsg mtid) <| UnitForum.setup id mtid
    Page.UnitElections -> Cmd.none

--------------------------------------------------------------------------------
-- Update

type Msg
  = GetUnitInfo Page.UnitTab (API.Response UnitInfo)
  | SetTab Page.UnitTab
  | ResetNotification
  | UnitInfoMsg UnitInfo.Msg
  | UnitAdminMsg UnitAdmin.Msg
  | UnitFilesMsg UnitFiles.Msg
  | UnitForumMsg (Maybe (Id Thread)) UnitForum.Msg

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse pageInfo ResetNotification model
  in
  case msg of
    UnitInfoMsg umsg -> WebData.update model model.info <| \info ->
      let
        (unitInfo, cmd) = UnitInfo.update info pageInfo umsg model.unitInfo
      in
        ( { model | unitInfo = unitInfo }
        , Cmd.map UnitInfoMsg cmd
        )

    UnitAdminMsg umsg -> WebData.update model model.info <| \info ->
      let
        (unitAdmin, cmd) = UnitAdmin.update info pageInfo umsg model.unitAdmin
      in
        ( { model | unitAdmin = unitAdmin }
        , Cmd.map UnitAdminMsg cmd
        )

    UnitFilesMsg umsg -> WebData.update model model.info <| \info ->
      let
        (unitFiles, cmd) = UnitFiles.update pageInfo info umsg model.unitFiles
      in
        ( { model | unitFiles = unitFiles }
        , Cmd.map UnitFilesMsg cmd
        )

    UnitForumMsg mtid umsg -> WebData.update model model.info <| \info ->
      let
        (unitForum, cmd) = UnitForum.update info mtid pageInfo umsg model.unitForum
      in
        ( { model | unitForum = unitForum }
        , Cmd.map (UnitForumMsg mtid) cmd
        )

    GetUnitInfo tab result -> onSuccess result <| \info ->
      ( let
          modelChoice = case model.info of
            WebData.Success old ->
              if old.id == info.id then model else initial
            _ ->
              initial
        in
          { modelChoice
          | info = WebData.Success info
          }
      , case tab of
          Page.UnitInfo -> Cmd.none
          _ -> setupTab info.id tab
      )

    SetTab tab -> WebData.update model model.info <| \info ->
      ( model
      , Page.goTo pageInfo <| Page.Unit tab info.id
      )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> Page.UnitTab -> List (Html Msg)
view model tab =
  Notification.with model.notification <|
  WebData.view model.info <| \info ->
  [ Html.div
      [ Html.Attributes.class "is-flex is-justify-content-space-between" ]
      [ Html.h1
          [ Html.Attributes.class "title" ]
          [ Html.text info.unit.name ]
      , Html.map SetTab <|
        Tabs.view (sameTab tab) <| List.concat
          [ if info.isMember
            then
              [ (Page.UnitInfo, "Information")
              , (Page.UnitForum Nothing, "Forum")
              , (Page.UnitElections, "Elections (" ++ String.fromInt info.electionsPending ++ ")")
              , (Page.UnitFiles, "Files")
              ]
            else []
          , if info.isAdmin
            then [(Page.UnitAdmin, "Admin")]
            else []
          ]
      ]
  , Html.div
      [ Html.Attributes.style "height" "100%"
      ] <| case tab of
      Page.UnitInfo ->
        List.map (Html.map UnitInfoMsg) <|
        UnitInfo.view info model.unitInfo
      Page.UnitAdmin ->
        List.map (Html.map UnitAdminMsg) <|
        UnitAdmin.view info model.unitAdmin
      Page.UnitForum mtid ->
        List.map (Html.map (UnitForumMsg mtid)) <|
        UnitForum.view info mtid model.unitForum
      Page.UnitFiles ->
        List.map (Html.map UnitFilesMsg) <|
        UnitFiles.view info model.unitFiles
      Page.UnitElections -> []
  ]

sameTab : Page.UnitTab -> Page.UnitTab -> Bool
sameTab t1 t2 =
  case (t1, t2) of
    (Page.UnitInfo, Page.UnitInfo) -> True
    (Page.UnitAdmin, Page.UnitAdmin) -> True
    (Page.UnitFiles, Page.UnitFiles) -> True
    (Page.UnitForum _, Page.UnitForum _) -> True
    _ -> False
