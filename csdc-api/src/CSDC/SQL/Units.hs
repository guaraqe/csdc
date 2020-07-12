{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.Units
  ( select
  , insert
  , update
  , delete
  ) where

import CSDC.DAO.Types (Unit (..))
import CSDC.Data.Id (Id (..))

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

select :: Statement (Id Unit) (Maybe Unit)
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT name, description, chair"
      , "FROM units"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $
      Unit <$>
        Decoder.text <*>
        Decoder.text <*>
        Decoder.id

insert :: Statement Unit (Id Unit)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO units (name, description, chair)"
      , "VALUES ($1, $2, $3)"
      , "RETURNING id"
      ]

    encoder =
      (contramap unit_name Encoder.text) <>
      (contramap unit_description Encoder.text) <>
      (contramap unit_chair Encoder.id)

    decoder = Decoder.singleRow Decoder.id

update :: Statement (Id Unit, Unit) ()
update = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE units"
      , "VALUES (name = $2, description = $3, chair = $4)"
      , "WHERE id = $1"
      ]

    encoder =
      (contramap fst Encoder.id) <>
      (contramap (unit_name . snd) Encoder.text) <>
      (contramap (unit_description . snd) Encoder.text) <>
      (contramap (unit_chair . snd) Encoder.id)

    decoder = Decoder.noResult

delete :: Statement (Id Unit) ()
delete = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM units"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult