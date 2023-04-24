module CSDC.Daemon.Election
  ( daemon,
  )
where

import CSDC.Action
import CSDC.DAO
import CSDC.Daemon (Daemon)
import CSDC.Daemon qualified as Daemon
import CSDC.SQL.Elections qualified as SQL.Elections
import Control.Monad (forM_)

-- | A daemon that checks the e-mail queue and tries to send them. If
-- successful, remove them from the queue.
daemon :: Daemon (Action user)
daemon = Daemon.make 5 "Election" $ do
  pairs <- runQuery SQL.Elections.selectResolvableElections ()

  forM_ pairs $ \(electionId, election) -> do
    summary <- getElectionSummary electionId election

    let result = getElectionResult summary

    runQuery SQL.Elections.updateElectionResult (electionId, result)
