{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Update.VoteId
       ( VoteId
       , mkVoteId
       ) where

import           Universum

import qualified Formatting as F
import           Formatting.Buildable (Buildable (build))

import           Formatting (bprint, (%))

import           Pos.Crypto (PublicKey)

import           Pos.Core.Update.Proposal (UpId)
import           Pos.Core.Update.Vote (UpdateVote (..))

type VoteId = (UpId, PublicKey, Bool)

instance Buildable VoteId where
    build (upId, pk, dec) =
      bprint ("Vote Id { voter: "%F.build%", proposal id: "%F.build%", voter's decision: "%F.build%" }")
             pk upId dec

mkVoteId :: UpdateVote -> VoteId
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)
