module Validation exposing
  ( Validation
  , make
  , validate
  , valid
  , invalid
  , map
  , andMap
  )

type Validation a = Validation (Result (List String) a)

make : Result (List String) a -> Validation a
make r = Validation r

validate : Validation a -> Result (List String) a
validate (Validation r) = r

valid : a -> Validation a
valid a = Validation (Ok a)

invalid : String -> Validation a
invalid s = Validation (Err [s])

map : (a -> b) -> Validation a -> Validation b
map f (Validation r) = Validation (Result.map f r)

andMap : Validation a -> Validation (a -> b) -> Validation b
andMap (Validation r) (Validation f) = Validation <|
  case r of
   Err er ->
     case f of
       Err ef -> Err (er ++ ef)
       Ok _ -> Err er
   Ok a ->
     case f of
       Err ef -> Err ef
       Ok g -> Ok (g a)
