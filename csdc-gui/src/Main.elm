module Main exposing (..)

import CSDC.API as API
import CSDC.Component.Admin as Admin
import CSDC.Component.Explorer as Explorer
import CSDC.Component.InvitationMember as InvitationMember
import CSDC.Component.Menu as Menu
import CSDC.Component.MessageMember as MessageMember
import CSDC.Component.MessageSubpart as MessageSubpart
import CSDC.Component.PreviewMessage as PreviewMessage
import CSDC.Component.PreviewReply as PreviewReply
import CSDC.Component.ReplyMember as ReplyMember
import CSDC.Component.ReplySubpart as ReplySubpart
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
  , messageMember : MessageMember.Model
  , invitationMember : InvitationMember.Model
  , replyMember : ReplyMember.Model
  , messageSubpart : MessageSubpart.Model
  , replySubpart : ReplySubpart.Model
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
      , messageMember = MessageMember.initial
      , invitationMember = InvitationMember.initial
      , replyMember = ReplyMember.initial
      , messageSubpart = MessageSubpart.initial
      , replySubpart = ReplySubpart.initial
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
  | MessageMemberMsg MessageMember.Param MessageMember.Msg
  | InvitationMemberMsg InvitationMember.Param InvitationMember.Msg
  | ReplyMemberMsg ReplyMember.Param ReplyMember.Msg
  | MessageSubpartMsg MessageSubpart.Param MessageSubpart.Msg
  | ReplySubpartMsg ReplySubpart.Param ReplySubpart.Msg
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
            , Cmd.map ViewUnitMsg <| ViewUnit.setup uid
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

          Studio.PreviewMessageMemberMsg (PreviewMessage.Reply r) ->
            ( { newModel
              | menu = Menu.ReplyMember r.message r.messageType
              }
            , Cmd.none
            )

          Studio.PreviewMessageSubpartMsg (PreviewMessage.Reply r) ->
            ( { newModel
              | menu = Menu.ReplySubpart r.message r.messageType
              }
            , Cmd.none
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
          ViewPerson.MessageMember pid ->
            case model.info of
              Just (User user) ->
                ( { model
                  | menu = Menu.InvitationMember pid user
                  }
                , Cmd.none
                )
              _ ->
                ( model
                , Cmd.none
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
            , Cmd.map ViewPersonMsg <|
              ViewPerson.setup id
            )
          ViewUnit.ViewAdmin id ->
            ( { model | menu = Menu.ViewUnitAdmin }
            , Cmd.map ViewUnitAdminMsg <|
              ViewUnitAdmin.setup id
            )
          ViewUnit.MessageMember pid uid mtype ->
            ( { model | menu = Menu.MessageMember pid uid mtype }
            , Cmd.none
            )
          ViewUnit.MessageSubpart pid uid mtype ->
            ( { model | menu = Menu.MessageSubpart pid uid mtype }
            , Cmd.none
            )

          _ ->
            ( { model | viewUnit = viewUnit }
            , Cmd.map ViewUnitMsg cmd
            )

    ViewUnitAdminMsg m ->
      case m of
        ViewUnitAdmin.PreviewMessageMemberMsg (PreviewMessage.Reply r) ->
          ( { model
            | menu = Menu.ReplyMember r.message r.messageType
            }
          , Cmd.none
          )

        ViewUnitAdmin.PreviewMessageSubpartMsg (PreviewMessage.Reply r) ->
          ( { model
            | menu = Menu.ReplySubpart r.message r.messageType
            }
          , Cmd.none
          )
        _ ->
          let
            (viewUnitAdmin, cmd) = ViewUnitAdmin.update m model.viewUnitAdmin
          in
            ( { model | viewUnitAdmin = viewUnitAdmin }
            , Cmd.map ViewUnitAdminMsg cmd
            )

    MessageMemberMsg p m ->
      case m of
        MessageMember.Reset ->
          ( { model
            | messageMember = MessageMember.initial
            , menu = Menu.ViewUnit
            }
          , Cmd.map ViewUnitMsg (ViewUnit.setup p.unit.id)
          )
        _ ->
          let
            (messageMember, cmd) = MessageMember.update m p model.messageMember
          in
            ( { model | messageMember = messageMember }
            , Cmd.map (MessageMemberMsg p) cmd
            )

    InvitationMemberMsg p m ->
      case m of
        InvitationMember.Reset ->
          ( { model
            | invitationMember = InvitationMember.initial
            , menu = Menu.ViewPerson
            }
          , Cmd.none
          )
        _ ->
          let
            (invitationMember, cmd) = InvitationMember.update m p model.invitationMember
          in
            ( { model | invitationMember = invitationMember }
            , Cmd.map (InvitationMemberMsg p) cmd
            )

    ReplyMemberMsg p m ->
      case m of
        ReplyMember.Reset ->
          ( { model
            | replyMember = ReplyMember.initial
            , menu = Menu.Studio
            }
          , case model.info of
              Just (User pinfo) ->
                Cmd.map StudioMsg <| Studio.setup pinfo.id
              _ ->
                Cmd.none
          )
        _ ->
          let
            (replyMember, cmd) = ReplyMember.update m p model.replyMember
          in
            ( { model | replyMember = replyMember }
            , Cmd.map (ReplyMemberMsg p) cmd
            )

    MessageSubpartMsg p m ->
      case m of
        MessageSubpart.Reset ->
          ( { model
            | messageSubpart = MessageSubpart.initial
            , menu = Menu.ViewUnit
            }
          , Cmd.map ViewUnitMsg (ViewUnit.setup p.unit.id)
          )
        _ ->
          let
            (messageSubpart, cmd) = MessageSubpart.update m p model.messageSubpart
          in
            ( { model | messageSubpart = messageSubpart }
            , Cmd.map (MessageSubpartMsg p) cmd
            )

    ReplySubpartMsg p m ->
      case m of
        ReplySubpart.Reset ->
          ( { model
            | replySubpart = ReplySubpart.initial
            , menu = Menu.Studio
            }
          , case model.info of
              Just (User pinfo) ->
                Cmd.map StudioMsg <| Studio.setup pinfo.id
              _ ->
                Cmd.none
          )
        _ ->
          let
            (replySubpart, cmd) = ReplySubpart.update m p model.replySubpart
          in
            ( { model | replySubpart = replySubpart }
            , Cmd.map (ReplySubpartMsg p) cmd
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

      Menu.MessageMember pid uid mtype ->
        let
          param = {person = pid, unit = uid, messageType = mtype}
          toMsg = MessageMemberMsg param
        in
          List.map (Element.map toMsg) <|
          [ MessageMember.view param model.messageMember ]

      Menu.InvitationMember pid user ->
        let
          param = {person = pid, user = user}
          toMsg = InvitationMemberMsg param
        in
          List.map (Element.map toMsg) <|
          [ InvitationMember.view param model.invitationMember ]

      Menu.ReplyMember mid mtype ->
        let
          param = {message = mid, messageType = mtype}
          toMsg = ReplyMemberMsg param
        in
          List.map (Element.map toMsg) <|
          [ ReplyMember.view param model.replyMember ]

      Menu.MessageSubpart pinfo uinfo mtype ->
        let
          param = {person = pinfo, unit = uinfo, messageType = mtype}
          toMsg = MessageSubpartMsg param
        in
          List.map (Element.map toMsg) <|
          [ MessageSubpart.view param model.messageSubpart ]

      Menu.ReplySubpart mid mtype ->
        let
          param = {message = mid, messageType = mtype}
          toMsg = ReplySubpartMsg param
        in
          List.map (Element.map toMsg) <|
          [ ReplySubpart.view param model.replySubpart ]

      Menu.Admin ->
        List.map (Element.map AdminMsg) <|
        Admin.view model.admin
