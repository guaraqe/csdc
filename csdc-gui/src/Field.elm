module Field exposing
  ( Field
  , Status
  , name
  , raw
  , set
  , errors
  , validate
    -- Common
  , make
  , required
  , requiredString
  , requiredId
  )

import CSDC.Types exposing (Id (..))
import Validation exposing (Validation)

type Status b
  -- The field was incorrectly parsed.
  = Invalid (List String)
  -- The field was correctly parsed.
  | Valid b
  -- Exists so that initial values do not show errors from the start.
  | Initial (List String)

type Field a b = Field
  { -- Name of the field, used for UI and errors.
    name : String
    -- The value that was input by the user.
  , raw : a
    -- The validation status of the field.
  , status : Status b
    -- The validation function of the field.
  , validate : a -> Validation b
  }

name : Field a b -> String
name (Field f) = f.name

raw : Field a b -> a
raw (Field f) = f.raw

set : a -> Field a b -> Field a b
set a (Field f) = Field
  { name = f.name
  , raw = a
  , status =
      case Validation.validate (f.validate a) of
        Err e -> Invalid e
        Ok b -> Valid b
  , validate = f.validate
  }

errors : Field a b -> List String
errors (Field f) =
  case f.status of
    Invalid e -> e
    _ -> []

validate : Field a b -> Validation b
validate (Field f) =
  let
    addPrefix s = f.name ++ ": " ++ s
  in
    case f.status of
      Invalid e -> Validation.make <| Err (List.map addPrefix e)
      Valid a -> Validation.make <| Ok a
      Initial e -> Validation.make <| Err (List.map addPrefix e)

--------------------------------------------------------------------------------
-- Fields

make : String -> a -> (a -> Validation b) -> Field a b
make n r v = Field
  { name = n
  , raw = r
  , validate = v
  , status =
      case Validation.validate (v r) of
        Err e -> Initial e
        Ok b -> Valid b
  }

required : String -> Field (Maybe a) a
required n = make n Nothing <| \m ->
  case m of
    Nothing -> Validation.invalid "Field is required."
    Just a -> Validation.valid a

requiredString : String -> Field String String
requiredString n = make n "" <| \s ->
  if String.isEmpty s
  then Validation.invalid "String should not be empty."
  else Validation.valid s

requiredId : String -> Field String (Id a)
requiredId n = make n "" <| \s ->
  case String.toInt s of
    Nothing -> Validation.invalid "This is not a valid Id."
    Just k -> Validation.valid (Id k)
