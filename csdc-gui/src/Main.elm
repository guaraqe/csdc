module Main exposing (..)

import CSDC.API as API
import CSDC.Component.Admin as Admin
import CSDC.Component.Explorer as Explorer
import CSDC.Component.Menu as Menu
import CSDC.Component.Studio as Studio
import CSDC.Component.ViewPerson as ViewPerson
import CSDC.Component.ViewUnit as ViewUnit
import CSDC.Component.ViewUnitAdmin as ViewUnitAdmin
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

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
-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { info : Maybe (User PersonInfo)
  , menu : Menu.Model
  , admin : Admin.Model
  , viewPerson : ViewPerson.Model
  , viewUnit : ViewUnit.Model
  , viewUnitAdmin : ViewUnitAdmin.Model
  , explorer : Explorer.Model
  , studio : Studio.Model
  , notification : Notification
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    (explorer, explorerCmd) = Explorer.initial ()
  in
    ( { info = Nothing
      , menu = Menu.initial
      , explorer = explorer
      , admin = Admin.initial
      , studio = Studio.initial
      , viewPerson = ViewPerson.initial
      , viewUnit = ViewUnit.initial
      , viewUnitAdmin = ViewUnitAdmin.initial
      , notification = Notification.Empty
      }
    , Cmd.batch
        [ Cmd.map ExplorerMsg explorerCmd
        , Cmd.map APIMsg API.rootPerson
        ]
    )

--------------------------------------------------------------------------------
-- Update

type Msg
  = AdminMsg Admin.Msg
  | MenuMsg Menu.Msg
  | ExplorerMsg Explorer.Msg
  | ViewPersonMsg ViewPerson.Msg
  | ViewUnitMsg ViewUnit.Msg
  | ViewUnitAdminMsg ViewUnitAdmin.Msg
  | StudioMsg Studio.Msg
  | APIMsg API.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AdminMsg m ->
      let
        (admin, cmd) = Admin.update m model.admin
      in
        ( { model | admin = admin }
        , Cmd.map AdminMsg cmd
        )

    MenuMsg m ->
      let
        menu = Menu.update m model.menu
      in
        ( { model | menu = menu }
        , Cmd.none
        )

    ExplorerMsg m ->
      case m of
        Explorer.ViewUnit uid ->
            ( { model | menu = Menu.ViewUnit }
            , Cmd.map (ViewUnitMsg << ViewUnit.APIMsg) (API.getUnitInfo uid)
            )
        _ ->
          let
            (explorer, cmd) = Explorer.update m model.explorer
          in
            ( { model | explorer = explorer }
            , Cmd.map ExplorerMsg cmd
            )

    StudioMsg m ->
      let
        (studio, cmd) = Studio.update m model.studio
        (newModel, newCmd) =
          ( { model | studio = studio }
          , Cmd.map StudioMsg cmd
          )
      in
        case m of
          Studio.View (Studio.ViewSelectedUnit uid) ->
            ( { newModel | menu = Menu.ViewUnit }
            , Cmd.map (ViewUnitMsg << ViewUnit.APIMsg) (API.getUnitInfo uid)
            )

          Studio.APIMsg (API.CreateUnit result) ->
            case result of
              Err err ->
                ( { model | notification = Notification.HttpError err }
                , Cmd.none
                )
              Ok member ->
                ( { newModel | menu = Menu.ViewUnit }
                , Cmd.map (ViewUnitMsg << ViewUnit.APIMsg)
                    <| API.getUnitInfo (getMemberUnit member.value)
                )

          _ -> (newModel, newCmd)

    ViewPersonMsg m ->
      let
        (viewPerson, cmd) = ViewPerson.update m model.viewPerson
      in
        case m of
          ViewPerson.ViewSelected id ->
            ( { model | menu = Menu.ViewUnit }
            , Cmd.map (ViewUnitMsg << ViewUnit.APIMsg) <|
              API.getUnitInfo id
            )
          _ ->
            ( { model | viewPerson = viewPerson }
            , Cmd.map ViewPersonMsg cmd
            )

    ViewUnitMsg m ->
      let
        (viewUnit, cmd) = ViewUnit.update m model.viewUnit
      in
        case m of
          ViewUnit.View (ViewUnit.ViewSelectedPerson id) ->
            ( { model | menu = Menu.ViewPerson }
            , Cmd.map (ViewPersonMsg << ViewPerson.APIMsg) <|
              API.selectPerson id
            )
          ViewUnit.ViewAdmin id ->
            ( { model | menu = Menu.ViewUnitAdmin }
            , Cmd.map (ViewUnitAdminMsg << ViewUnitAdmin.APIMsg) <|
              API.unitInbox id
            )
          _ ->
            ( { model | viewUnit = viewUnit }
            , Cmd.map ViewUnitMsg cmd
            )

    ViewUnitAdminMsg m ->
      let
        (viewUnitAdmin, cmd) = ViewUnitAdmin.update m model.viewUnitAdmin
      in
        ( { model | viewUnitAdmin = viewUnitAdmin }
        , Cmd.map ViewUnitAdminMsg cmd
        )

    APIMsg m ->
      case m of
        API.RootPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok id ->
              case id of
                Admin ->
                  ( { model | info = Just Admin }
                  , Cmd.none
                  )
                User pid ->
                  ( model
                  , Cmd.batch
                      [ Cmd.map StudioMsg <| Studio.setup pid
                      , Cmd.map APIMsg <| API.getPersonInfo pid
                      ]
                  )

        API.GetPersonInfo result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok info ->
              ( { model | info = Just (User info) }
              , Cmd.none
              )


        _ -> (model, Cmd.none)

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--------------------------------------------------------------------------------
-- View

view : Model -> Html Msg
view model =
  layout [] <|
    row [ height fill, width fill ]
        [ menuPanel model
        , mainPanel model
        ]

menuPanel : Model -> Element Msg
menuPanel model =
  Element.map MenuMsg <| Menu.view model.menu

mainPanel : Model -> Element Msg
mainPanel model =
  column
    [ height fill
    , width <| fillPortion 5
    , spacing 10
    , padding 10
    ] <|
    case model.menu of
      Menu.Studio ->
        List.map (Element.map StudioMsg) <|
        Studio.view model.studio

      Menu.Explorer ->
        List.map (Element.map ExplorerMsg) <|
        Explorer.view model.explorer

      Menu.ViewPerson ->
        List.map (Element.map ViewPersonMsg) <|
        ViewPerson.view model.viewPerson

      Menu.ViewUnit ->
        List.map (Element.map ViewUnitMsg) <|
        ViewUnit.view model.info model.viewUnit

      Menu.ViewUnitAdmin ->
        List.map (Element.map ViewUnitAdminMsg) <|
        ViewUnitAdmin.view model.info model.viewUnitAdmin

      Menu.Admin ->
        List.map (Element.map AdminMsg) <|
        Admin.view model.admin
