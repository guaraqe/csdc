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

    encoder =
      contramap fst Encoder.id
        <> contramap ((.title) . snd) Encoder.text
        <> contramap ((.description) . snd) Encoder.text
        <> contramap ((.choices) . snd) Encoder.electionChoiceList
        <> contramap ((.electionType) . snd) Encoder.electionType
        <> contramap ((.visibleVotes) . snd) Encoder.bool
        <> contramap ((.endingAt) . snd) Encoder.posixTime

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
      choices <- Decoder.electionChoiceList
      electionType <- Decoder.electionType
      visibleVotes <- Decoder.bool
      endingAt <- Decoder.posixTime
      result <- Decoder.electionChoiceNullable
      resultComputedAt <- Decoder.posixTimeNullable
      let election = Election {..}
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
        [ "INSERT INTO voters (election, person, vote)",
          "VALUES ($1, $2, NULL)"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.noResult

insertVote :: Statement (Id Election, VotePayload) (Id Vote)
insertVote = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO votes (election, vote)",
          "VALUES ($1, $2)",
          "RETURNING id"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.votePayload

    decoder = Decoder.singleRow Decoder.id
