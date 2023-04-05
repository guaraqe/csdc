{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.Elections
  ( insertElection,
    selectElections,
    deleteElection,
    insertVoter,
    insertVote,
  )
where

import CSDC.Prelude
import CSDC.Types.Election
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

insertElection :: Statement (Id Unit, NewElection) (Id Election)
insertElection = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO elections (unit, title, description, choices, election_type, visible_votes, ending_at, result, result_computed_at)",
          "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)",
          "RETURNING id"
        ]

    -- XXX: fields have to be written like in the type definition, see
    -- Types.Election
    encoder =
      contramap (.unitId) Encoder.id
        <> contramap (.title) Encoder.text
        <> contramap (.description) Encoder.text
        <> contramap (.choices) Encoder.text
        <> contramap (.election_type) Encoder.text
        <> contramap (.visible_votes) Encoder.text
        <> contramap (.ending_at) Encoder.text
        <> contramap (.result) Encoder.text
        <> contramap (.result_computed_at) Encoder.text

    decoder = Decoder.singleRow Decoder.id

selectElections :: Statement (Id Unit, Id Person) [ElectionInfo]
selectElections = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT unit, title, description, choices, election_type, visible_votes, ending_at, result, result_computed_at",
          "FROM elections",
          "WHERE id = $1"
        ]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.id

    decoder = Decoder.rowList $ do
      unit <- Decoder.text
      title <- Decoder.text
      description <- Decoder.text
      choices <- Decoder.text
      election_type <- Decoder.text
      visible_votes <- Decoder.text
      ending_at <- Decoder.posixTime
      result <- Decoder.text
      result_computed_at <- Decoder.posixTime
      -- XXX: missing the info
      pure ElectionInfo {..}

deleteElection :: Statement (Id Election) ()
deleteElection = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM elections",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult

insertVoter :: Statement (Id Election, Id Person) ()
insertVoter = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO voters (election, person, voted_at, vote)",
          "VALUES ($1, $2, $3, $4)"
        ]

    encoder =
      (contramap (.electionId) Encoder.id)
        <> (contramap (.personId) Encoder.id)
        <> (contramap (.voted_at) Encoder.text)
        <> (contramap (.voteId) Encoder.id)

    decoder = Decoder.noResult

insertVote :: Statement (Id Election, NewVote) (Id Vote)
insertVote = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO votes (election, vote)",
          "VALUES ($1, $2)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.electionId) Encoder.id)
        <> (contramap (.voted_at) Encoder.text)

    decoder = Decoder.singleRow Decoder.id
