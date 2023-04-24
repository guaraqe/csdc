{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.Members
  ( selectByPerson,
    selectByUnit,
    insert,
    delete,
    deleteUnit,
    isIndirectMember,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ (sqlqq)
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

selectByPerson :: Statement (Id Person) [PersonMember]
selectByPerson = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT members.id, unit, units.name, units.description, units.chair, units.image, units.created_at",
          "FROM members",
          "JOIN units ON units.id = unit",
          "WHERE person = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      memberId <- Decoder.id
      unitId <- Decoder.id
      unit <- do
        name <- Decoder.text
        description <- Decoder.text
        chairId <- Decoder.id
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Unit {..}
      pure PersonMember {..}

selectByUnit :: Statement (Id Unit) [UnitMember]
selectByUnit = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT members.id, person, persons.name, persons.description, persons.email, persons.image, persons.created_at",
          "FROM members",
          "JOIN persons ON persons.id = person",
          "WHERE unit = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      memberId <- Decoder.id
      personId <- Decoder.id
      person <- do
        name <- Decoder.text
        description <- Decoder.text
        email <- Decoder.text
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Person {..}
      pure UnitMember {..}

insert :: Statement NewMember (Id Member)
insert = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO members (person, unit)",
          "VALUES ($1, $2)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.personId) Encoder.id)
        <> (contramap (.unitId) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

delete :: Statement (Id Member) ()
delete = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM members",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult

deleteUnit :: Statement (Id Unit) ()
deleteUnit = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM members",
          "WHERE unit = $1"
        ]

    encoder = Encoder.id
    decoder = Decoder.noResult

isIndirectMember :: Statement (Id Person, Id Unit) Bool
isIndirectMember = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        SELECT EXISTS (
          WITH RECURSIVE descendants(parent, child) AS (
              SELECT parent, child
              FROM subparts
              UNION
              SELECT d.parent, s.child
              FROM descendants d JOIN subparts s ON d.child = s.parent
          )
          SELECT *
          FROM descendants JOIN members ON descendants.child = members.unit
          WHERE descendants.parent = $2 AND members.person = $1
        )
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.singleRow Decoder.bool
