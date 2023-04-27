module Main exposing (main)

import API
import Http
import UI.Menu as Menu
import UI.Navbar as Navbar
import UI.Search as Search
import Notification exposing (Notification)
import Page as Page exposing (Page)
import Types exposing (..)
import Page.Explorer as Explorer
import Page.SignIn as SignIn
import Page.Studio as Studio
import Page.Person as Person
import Page.Unit as Unit

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes
import List
import Url
import Task
import Time

--------------------------------------------------------------------------------
-- Main

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = UrlRequested
    }

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { key: Nav.Key
  , url: Url.Url
  , page : Page
  , isLogged : Bool
  , timeZone : Time.Zone
  , viewPerson : Person.Model
  , viewUnit : Unit.Model
  , signin : SignIn.Model
  , explorer : Explorer.Model
  , studio : Studio.Model
  , search : Search.Model
  , notification : Notification
  }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  let
    (page, cmd) = route { key = key, url = url } Page.Studio
  in
    ( { key = key
      , url = url
      , page = page
      , timeZone = Time.utc
      , isLogged = False
      , signin = SignIn.initial
      , explorer = Explorer.initial
      , studio = Studio.initial
      , viewPerson = Person.initial
      , viewUnit = Unit.initial
      , search = Search.initial
      , notification = Notification.Empty
      }
    , Cmd.batch
        [ cmd
        , Cmd.map SignInCheck API.getUserInfo
        , Task.perform SetTimeZone Time.here
        ]
    )

--------------------------------------------------------------------------------
-- Update

type Msg
  = UrlChanged Url.Url
  | UrlRequested Browser.UrlRequest
  | SignIn
  | SignInCheck (API.Response PersonInfo)
  | SignOut
  | SignOutResult (API.Response ())
  | MenuMsg Menu.Msg
  | SignInMsg SignIn.Msg
  | ExplorerMsg Explorer.Msg
  | PersonMsg Person.Msg
  | UnitMsg Unit.Msg
  | StudioMsg Studio.Msg
  | SearchMsg Search.Msg
  | SetTimeZone Time.Zone

routeCmd : Page -> Page -> Cmd Msg
routeCmd prev page =
  case (prev, page) of
    (_, Page.SignIn) ->
      Cmd.none

    (_, Page.Studio) ->
      Cmd.map StudioMsg Studio.setup

    (_, Page.Explorer) ->
      Cmd.map ExplorerMsg Explorer.setup

    (_, Page.Person uid) ->
      Cmd.map PersonMsg (Person.setup uid)

    (Page.Unit tabPrev uidPrev, Page.Unit tab uid) ->
      Cmd.map UnitMsg <|
      if uid /= uidPrev
      then
        Unit.setup uid tab
      else
        Unit.changeTab uid tabPrev tab

    (_, Page.Unit tab uid) ->
      Cmd.map UnitMsg (Unit.setup uid tab)

route : Page.Info -> Page -> (Page, Cmd Msg)
route info prev =
  case info.url.fragment of
    Nothing ->
      (Page.Studio, Page.goTo info Page.Studio)
    Just fragment ->
      let
        page = Page.fromFragment fragment
      in
        (page, routeCmd prev page)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    pageInfo = { key = model.key, url = model.url }
  in
  case msg of
    UrlRequested urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      let
        (page, cmd) = route { key = model.key, url = url } model.page
      in
        ( { model | page = page, url = url }, cmd )

    SignIn ->
      ( model
      , Page.goTo pageInfo Page.SignIn
      )

    SignInCheck response ->
      case response of
        Err err ->
          case err of
            Http.BadStatus 401 ->
              ( { model | page = Page.SignIn }
              , Cmd.none
              )
            _ ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
        Ok _ ->
          ( { model | isLogged = True }
          , Cmd.none
          )

    SignOut ->
      ( model
      , Cmd.map SignOutResult API.signout
      )

    SignOutResult _ ->
      ( { model | isLogged = False }
      , Page.goTo pageInfo Page.SignIn
      )

    MenuMsg (Menu.SetItem menu) ->
      let
        page = Menu.toPage menu
      in
        ( { model | page = page }
        , Page.goTo pageInfo page
        )

    SignInMsg m ->
      let
        (signin, cmd, finished) = SignIn.update pageInfo m model.signin
      in
        ( { model
          | signin = signin
          , isLogged = if finished then True else model.isLogged
          }
        , Cmd.map SignInMsg cmd
        )

    ExplorerMsg m ->
      let
        (explorer, cmd) = Explorer.update pageInfo m model.explorer
      in
        ( { model | explorer = explorer }
        , Cmd.map ExplorerMsg cmd
        )

    StudioMsg m ->
      let
        (studio, cmd) = Studio.update pageInfo m model.studio
      in
        ( { model | studio = studio }
        , Cmd.map StudioMsg cmd
        )

    PersonMsg m ->
      let
        (viewPerson, cmd) = Person.update pageInfo m model.viewPerson
      in
        ( { model | viewPerson = viewPerson }
        , Cmd.map PersonMsg cmd
        )

    UnitMsg m ->
      let
        (viewUnit, cmd) = Unit.update model.timeZone pageInfo m model.viewUnit
      in
        ( { model | viewUnit = viewUnit }
        , Cmd.map UnitMsg cmd
        )

    SearchMsg m ->
      let
        (search, cmd) = Search.update pageInfo m model.search
      in
        ( { model | search = search }
        , Cmd.map SearchMsg cmd
        )

    SetTimeZone timeZone -> ( { model | timeZone = timeZone }, Cmd.none )

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map UnitMsg <|
  Unit.subscriptions model.timeZone model.viewUnit

--------------------------------------------------------------------------------
-- View

view : Model -> Browser.Document Msg
view model =
  { title = "DAO"
  , body =
      [ Navbar.view model.isLogged SignIn SignOut <|
        Html.map SearchMsg <| Search.view model.search
      , Html.div
          [ if model.isLogged && model.page /= Page.SignIn
            then Html.Attributes.class "columns"
            else Html.Attributes.class "columns is-centered"
          , Html.Attributes.attribute "style" "padding:25px; height: calc(100vh - 80px)"
          ]
          ( if not model.isLogged || model.page == Page.SignIn
            then
            [ Html.div
                [ Html.Attributes.class "column is-one-quarter" ]
                (viewMain model)
            ]
            else
            [ Html.div
                [ Html.Attributes.class "column is-one-fifth"
                ]
                [ viewMenu model
                ]
            , Html.div
                [ Html.Attributes.class "column is-four-fifths"
                ]
                [ Html.div
                    [ Html.Attributes.class "content"
                    , Html.Attributes.style "height" "100%"
                    ]
                    (viewMain model)
                ]
            ]
          )
      ]
  }

viewMenu : Model -> Html Msg
viewMenu model =
  Html.map MenuMsg <| Menu.view (Menu.fromPage model.page)

viewMain : Model -> List (Html Msg)
viewMain model =
  case model.page of
    Page.Studio ->
      List.map (Html.map StudioMsg) <|
      Studio.view model.studio

    Page.SignIn ->
      List.map (Html.map SignInMsg) <|
      SignIn.view model.signin

    Page.Explorer ->
      List.map (Html.map ExplorerMsg) <|
      Explorer.view model.explorer

    Page.Person _ ->
      List.map (Html.map PersonMsg) <|
      Person.view model.viewPerson

    Page.Unit tab _ ->
      List.map (Html.map UnitMsg) <|
      Unit.view model.timeZone model.viewUnit tab
