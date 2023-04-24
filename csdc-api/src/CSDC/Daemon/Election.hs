{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module CSDC.Daemon.Election
  ( daemon,
  )
where

import CSDC.Action
import CSDC.Daemon (Daemon)
import CSDC.Daemon qualified as Daemon
import CSDC.SQL.Elections qualified as SQL.Elections
import CSDC.Types.Election
import Control.Monad (forM_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Monoid (Sum (..))

-- | A daemon that checks the e-mail queue and tries to send them. If
-- successful, remove them from the queue.
daemon :: Daemon (Action user)
daemon = Daemon.make 5 "Election" $ do
  pairs <- runQuery SQL.Elections.selectResolvableElections ()

  forM_ pairs $ \(electionId, election) -> do

    votes <- runQuery SQL.Elections.selectElectionVotes electionId

    let toGradedVote :: VotePayload -> HashMap ElectionChoice (Sum Int)
        toGradedVote vote = case election.electionType of
          MajorityConsensus ->
            let
              fromGrade = \case
                GradeExcellent -> Sum 3
                GradeVeryGood -> Sum 2
                GradeGood -> Sum 1
                GradeAcceptable -> Sum 0
                GradeBad -> Sum (-1)
                GradeVeryBad -> Sum (-2)
            in
              case vote of
                VotePayloadMajorityConsensus grades -> fmap fromGrade grades
                _ -> mempty

          SimpleMajority ->
            let
              fromGrade = \case
                Nothing -> mempty
                Just choice -> HashMap.singleton choice (Sum 1)
            in
              case vote of
                VotePayloadSimpleMajority choice -> fromGrade choice
                _ -> mempty

        summary :: HashMap ElectionChoice (Sum Int)
        summary = mconcat $ fmap toGradedVote votes

        accumulate ::
          (Sum Int, [ElectionChoice]) ->
          ElectionChoice ->
          Sum Int ->
          (Sum Int, [ElectionChoice])
        accumulate (winnersGrade, winners) choice choiceGrade =
          if | winnersGrade == choiceGrade -> (winnersGrade, choice:winners)
             | winnersGrade < choiceGrade -> (choiceGrade, [choice])
             | otherwise -> (winnersGrade, winners)

        (_,wins) = HashMap.foldlWithKey accumulate (Sum (-1000),[]) summary

        result = case wins of
          [winner] -> Just winner
          _ -> Nothing

    runQuery SQL.Elections.updateElectionResult (electionId, result)
