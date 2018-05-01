{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO rename the module / move defintions / whatever.
-- It's not about the network at all.

module Pos.Update.Network.Listeners
       ( handleProposal
       , handleVote
       ) where

import           Universum

import           Formatting (sformat, (%))
import qualified Formatting as F
import           System.Wlog (WithLogger, logNotice, logWarning)

import           Pos.Core (ProtocolMagic)
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Logic.Local (processProposal, processVote)
import           Pos.Update.Mode (UpdateMode)

handleProposal
    :: forall ctx m . UpdateMode ctx m
    => ProtocolMagic
    -> (UpdateProposal, [UpdateVote])
    -> m Bool
handleProposal pm (proposal, votes) = do
    res <- processProposal pm proposal
    logProp proposal res
    let processed = isRight res
    processed <$ when processed (mapM_ processVoteLog votes)
  where
    processVoteLog :: UpdateVote -> m ()
    processVoteLog vote = processVote pm vote >>= logVote vote
    logVote vote (Left cause) =
        logWarning $ sformat ("Proposal is accepted but vote "%F.build%
                              " is rejected, the reason is: "%F.build)
                     vote cause
    logVote vote (Right _) = logVoteAccepted vote

    logProp prop (Left cause) =
        logWarning $ sformat ("Processing of proposal "%F.build%
                              " failed, the reason is: "%F.build)
              prop cause
    -- Update proposals are accepted rarely (at least before Shelley),
    -- so it deserves 'Notice' severity.
    logProp prop (Right _) =
        logNotice $ sformat ("Processing of proposal "%F.build%" is successful")
              prop

----------------------------------------------------------------------------
-- UpdateVote
----------------------------------------------------------------------------

handleVote
    :: UpdateMode ctx m
    => ProtocolMagic
    -> UpdateVote
    -> m Bool
handleVote pm uv = do
    res <- processVote pm uv
    logProcess uv res
    pure $ isRight res
  where
    logProcess vote (Left cause) =
        logWarning $ sformat ("Processing of vote "%F.build%
                              "failed, the reason is: "%F.build)
                     vote cause
    logProcess vote (Right _) = logVoteAccepted vote

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Update votes are accepted rarely (at least before Shelley), so
-- it deserves 'Notice' severity.
logVoteAccepted :: WithLogger m => UpdateVote -> m ()
logVoteAccepted =
    logNotice . sformat ("Processing of vote "%F.build%"is successfull")
