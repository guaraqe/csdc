module CSDC.Data.IdMap
  ( IdMap (..)
  , IdMap'
  , empty
  , lookup
  , find
  , insert
  , insertNew
  , update
  , delete
  , filter
  , keys
  ) where

import CSDC.Data.Id (Id (..), zero, next)

import Data.Aeson (ToJSON, FromJSON)
import Data.IntMap.Strict (IntMap)

import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap

import Prelude hiding (lookup, filter)

--------------------------------------------------------------------------------
-- Type definition

newtype IdMap a b = IdMap { getIdMap :: IntMap b }
  deriving newtype (Show, Eq, Functor, Foldable, ToJSON, FromJSON)
  deriving stock (Traversable)

type IdMap' a = IdMap a a

empty :: IdMap a b
empty = IdMap (IntMap.empty)

lookup :: Id a -> IdMap a b -> Maybe b
lookup (Id uid) (IdMap m) = IntMap.lookup uid m

find :: (b -> Bool) -> IdMap a b -> Maybe (Id a, b)
find p (IdMap m) =
  fmap (\(uid,a) -> (Id uid,a)) $
  List.find (p . snd) $
  IntMap.toList m

insert :: Id a -> b -> IdMap a b -> IdMap a b
insert (Id uid) a (IdMap m) = IdMap $ IntMap.insert uid a m

update :: Id a -> (b -> b) -> IdMap a b -> IdMap a b
update (Id uid) f (IdMap m) = IdMap $ IntMap.adjust f uid m

insertNew :: b -> IdMap a b -> (Id a, IdMap a b)
insertNew a idmap@(IdMap m) =
  let
    uid =
      case IntMap.lookupMax m of
        Nothing -> zero
        Just (uid',_) -> next (Id uid')
  in
    (uid, insert uid a idmap)

delete :: Id a -> IdMap a b -> IdMap a b
delete (Id uid) (IdMap m) = IdMap $ IntMap.delete uid m

filter :: (b -> Bool) -> IdMap a b -> IdMap a b
filter f (IdMap m) = IdMap $ IntMap.filter f m

keys :: IdMap a b -> [Id a]
keys (IdMap m) = Id <$> IntMap.keys m
