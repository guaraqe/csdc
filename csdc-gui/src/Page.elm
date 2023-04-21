module Page exposing
  ( Page (..)
  , UnitTab (..)
  , Info
  , goTo
  , reload
  , fromFragment
  )

import Types exposing (..)

import Browser.Navigation as Nav
import Maybe
import String
import Url exposing (Url)

--------------------------------------------------------------------------------
-- Model

type Page
  = SignIn
  | Explorer
  | Studio
  | Person (Id Person)
  | Unit UnitTab (Id Unit)

type UnitTab = UnitInfo | UnitAdmin | UnitElections | UnitFiles | UnitForum (Maybe (Id Thread))

type alias Info =
  { key : Nav.Key
  , url : Url
  }

goTo : Info -> Page -> Cmd msg
goTo { key, url } page =
  let
    newUrl =
      { url
      | fragment = Just (toFragment page)
      }
  in
    Nav.pushUrl key (Url.toString newUrl)

reload : Cmd msg
reload = Nav.reload

--------------------------------------------------------------------------------
-- Conversion to URL fragment

toFragment : Page -> String
toFragment page = String.join "/" ("" :: toFragments page)

toFragments : Page -> List String
toFragments page =
  case page of
    SignIn -> ["signin"]
    Explorer -> ["explorer"]
    Studio -> ["studio"]
    Person uid -> ["person", toFragmentId uid]
    Unit tab uid -> ["unit", toFragmentId uid] ++
      case tab of
        UnitInfo -> []
        UnitAdmin -> ["admin"]
        UnitElections -> ["elections"]
        UnitFiles -> ["files"]
        UnitForum Nothing -> ["forum"]
        UnitForum (Just tid) -> ["forum", toFragmentId tid]

toFragmentId : Id a -> String
toFragmentId = idToString

--------------------------------------------------------------------------------
-- Conversion from URL fragment

fromFragment : String -> Page
fromFragment fragment =
  fromFragments <| List.drop 1 (String.split "/" fragment)

fromFragments : List String -> Page
fromFragments l =
  let default = Maybe.withDefault Studio in
  case l of
    ["signin"] -> SignIn
    ["studio"] -> Studio
    ["explorer"] -> Explorer
    ["person", uid] ->
      default <| Maybe.map Person (fromFragmentId uid)
    ["unit", uid] ->
      default <| Maybe.map (Unit UnitInfo) (fromFragmentId uid)
    ["unit", uid, "admin"] ->
      default <| Maybe.map (Unit UnitAdmin) (fromFragmentId uid)
    ["unit", uid, "elections"] ->
      default <| Maybe.map (Unit UnitElections) (fromFragmentId uid)
    ["unit", uid, "files"] ->
      default <| Maybe.map (Unit UnitFiles) (fromFragmentId uid)
    ["unit", uid, "forum"] ->
      default <| Maybe.map (Unit (UnitForum Nothing)) (fromFragmentId uid)
    ["unit", uid, "forum", tid] ->
      default <| Maybe.map2 (\t -> Unit (UnitForum (Just t))) (fromFragmentId tid) (fromFragmentId uid)
    _ -> Studio

fromFragmentId : String -> Maybe (Id a)
fromFragmentId = idFromString
