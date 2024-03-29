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
    selectResolvableElections,
    selectElectionVotes,
    updateElectionResult,
    selectPendingElections,
    selectElectionVisibleVotes,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ (sqlqq)
import CSDC.Types.Election
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement (Statement (..))

insertElection :: Statement (Id Unit, NewElection) (Id Election)
insertElection = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        INSERT INTO elections (
          unit,
          title,
          description,
          choices,
          election_type,
          visible_votes,
          ending_at,
          result,
          result_computed_at
        )
        VALUES ($1, $2, $3, $4, $5::election_type, $6, $7, NULL, NULL)
        RETURNING id
      |]

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
      [sqlqq|
        SELECT
          id,
          unit,
          title,
          description,
          choices,
          election_type,
          visible_votes,
          ending_at,
          result,
          result_computed_at,
          (SELECT voted_at FROM voters WHERE election = elections.id AND person = $2),
          (SELECT COUNT(*) FROM voters WHERE election = elections.id)
        FROM elections
        WHERE unit = $1
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.rowList $ do
      electionId <- Decoder.id
      election <- do
        unitId <- Decoder.id
        title <- Decoder.text
        description <- Decoder.text
        choices <- Decoder.electionChoiceList
        electionType <- Decoder.electionType
        visibleVotes <- Decoder.bool
        endingAt <- Decoder.posixTime
        result <- Decoder.electionChoiceNullable
        resultComputedAt <- Decoder.posixTimeNullable
        pure Election {..}
      votedAt <- Decoder.posixTimeNullable
      totalVotes <- Decoder.int
      pure ElectionInfo {..}

selectElectionVisibleVotes :: Statement (Id Election) (Maybe Bool)
selectElectionVisibleVotes = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        SELECT visible_votes
        FROM elections
        WHERE id = $1
      |]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe Decoder.bool

deleteElection :: Statement (Id Election) ()
deleteElection = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        DELETE FROM elections
        WHERE id = $1
      |]

    encoder = Encoder.id
    decoder = Decoder.noResult

insertVoter :: Statement (Id Election, Id Person, Maybe (Id Vote)) ()
insertVoter = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        INSERT INTO voters (election, person, vote, voted_at)
        VALUES ($1, $2, $3, NOW())
      |]

    encoder =
      contramap (\(a,_,_) -> a) Encoder.id
        <> contramap (\(_,a,_) -> a) Encoder.id
        <> contramap (\(_,_,a) -> a) Encoder.idNullable

    decoder = Decoder.noResult

insertVote :: Statement (Id Election, VotePayload) (Id Vote)
insertVote = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        INSERT INTO votes (election, vote)
        VALUES ($1, $2)
        RETURNING id
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.votePayload

    decoder = Decoder.singleRow Decoder.id

selectResolvableElections :: Statement () [(Id Election, Election)]
selectResolvableElections = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        SELECT
          id,
          unit,
          title,
          description,
          choices,
          election_type,
          visible_votes,
          ending_at,
          result,
          result_computed_at
        FROM elections
        WHERE ending_at < NOW() AND result_computed_at IS NULL
      |]

    encoder = Encoders.noParams

    decoder = Decoder.rowList $ do
      electionId <- Decoder.id
      election <- do
        unitId <- Decoder.id
        title <- Decoder.text
        description <- Decoder.text
        choices <- Decoder.electionChoiceList
        electionType <- Decoder.electionType
        visibleVotes <- Decoder.bool
        endingAt <- Decoder.posixTime
        result <- Decoder.electionChoiceNullable
        resultComputedAt <- Decoder.posixTimeNullable
        pure Election {..}
      pure (electionId, election)

selectElectionVotes :: Statement (Id Election) [VotePayload]
selectElectionVotes = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        SELECT vote
        FROM votes
        WHERE election = $1
      |]

    encoder = Encoder.id

    decoder = Decoder.rowList Decoder.votePayload

updateElectionResult :: Statement (Id Election, Maybe ElectionChoice) ()
updateElectionResult = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        UPDATE elections
        SET result = $2, result_computed_at = NOW()
        WHERE id = $1
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.electionChoiceNullable

    decoder = Decoders.noResult

selectPendingElections :: Statement (Id Unit, Id Person) Int
selectPendingElections = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
        SELECT
          ( SELECT COUNT(*)
            FROM elections
            WHERE elections.unit = $1 AND result_computed_at IS NULL
          ) -
          ( SELECT COUNT(*)
            FROM elections JOIN voters ON elections.id = voters.election
            WHERE elections.unit = $1 AND voters.person = $2 AND result_computed_at IS NULL
          )
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.singleRow Decoder.int
