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
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

insertElection :: Statement (Id Unit, NewElection) (Id Election)
insertElection = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      INSERT INTO election
        ???????
      VALUES
        ???????
      RETURNING
        id
      |]

    encoder = undefined

    decoder = undefined

selectElections :: Statement (Id Unit, Id Person) [ElectionInfo]
selectElections = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT
        ???????
      FROM
        elections
      WHERE
        ???????
      |]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.id

    decoder = undefined

deleteElection :: Statement (Id Election) ()
deleteElection = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      DELETE FROM elections
      WHERE ???????
      |]

    encoder = undefined

    decoder = Decoder.noResult

insertVoter :: Statement (Id Election, Id Person) ()
insertVoter = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      INSERT INTO voters
        ???????
      VALUES
        ???????
      |]

    encoder = undefined

    decoder = Decoder.noResult

insertVote :: Statement (Id Election, NewVote) (Id Vote)
insertVote = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      INSERT INTO voters
        ???????
      VALUES
        ???????
      RETURNING
        id
      |]

    encoder = undefined

    decoder = undefined
