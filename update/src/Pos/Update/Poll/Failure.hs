-- | Failures which can happen in Poll.

module Pos.Update.Poll.Failure
       ( PollVerFailure (..)
       , reportUnexpectedError
       ) where

import           Universum hiding (id, last)

import           Formatting (asInt, bprint, int, sformat, stext, (%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (build))
import           Serokell.Data.Memory.Units (Byte, memory)

import           Pos.Core (ApplicationName, BlockVersion, BlockVersionData,
                     Coin, EpochIndex, HeaderHash, NumSoftwareVersion,
                     ScriptVersion, StakeholderId, coinF)
import           Pos.Core.Update (BlockVersionModifier, UpAttributes, UpId)
import           Pos.Crypto (shortHashF)
import           Pos.Infra.Reporting (MonadReporting, reportError)

import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
----------------------------------------------------------------------------
-- Compat shims
----------------------------------------------------------------------------
-- pretty used to be in Universum
pretty :: Buildable a => a -> Text
pretty = toStrict . toLazyText . build

-- | PollVerFailure represents all possible errors which can
-- appear in Poll data verification.
data PollVerFailure
    =
      -- | 'BlockVersionModifier' for this 'BlockVersion' is already known and
      -- the one we saw doesn't match it.
      PollInconsistentBVM { pibExpected :: !BlockVersionModifier
                          , pibFound    :: !BlockVersionModifier
                          , pibUpId     :: !UpId}
    -- | 'BlockVersion' is already adopted and 'BlockVersionData' associated
    -- with it differs from the one we saw.
    | PollAlreadyAdoptedDiffers { paadAdopted  :: !BlockVersionData
                                , paadProposed :: !BlockVersionModifier
                                , paadUpId     :: !UpId}
    -- | Proposed script version must be the same as adopted one or
    -- greater by one, but this rule is violated.
    | PollWrongScriptVersion { pwsvAdopted  :: !ScriptVersion
                             , pwsvProposed :: !ScriptVersion
                             , pwsvUpId     :: !UpId}
    -- | A proposal tried to increase the block size limit more than it was
    -- allowed to
    | PollLargeMaxBlockSize { plmbsMaxPossible :: !Byte
                            , plmbsFound       :: !Byte
                            , plmbsUpId        :: !UpId}
    -- | A proposal attempted to change the end of the bootstrap era
    -- post factum
    | PollBootstrapEraInvalidChange { pbeicLast     :: !EpochIndex
                                    , pbeicAdopted  :: !EpochIndex
                                    , pbeicProposed :: !EpochIndex
                                    , pbeicUpId     :: !UpId }
    | PollNotFoundScriptVersion !BlockVersion
    | PollProposalAlreadyActive !UpId
    | PollSmallProposalStake { pspsThreshold :: !Coin
                            ,  pspsActual    :: !Coin
                            ,  pspsUpId      :: !UpId}
    | PollNotRichman { pnrStakeholder :: !StakeholderId
                    ,  pnrThreshold   :: !Coin
                    ,  pnrStake       :: !(Maybe Coin)}
    | PollUnknownProposal { pupStakeholder :: !StakeholderId
                         ,  pupProposal    :: !UpId}
    | PollUnknownStakes !EpochIndex
    | PollWrongSoftwareVersion { pwsvStored :: !(Maybe NumSoftwareVersion)
                              ,  pwsvApp    :: !ApplicationName
                              ,  pwsvGiven  :: !NumSoftwareVersion
                              ,  pwsvUpId   :: !UpId}
    | PollProposalIsDecided { ppidUpId        :: !UpId
                           ,  ppidStakeholder :: !StakeholderId}
    | PollExtraRevote { perUpId        :: !UpId
                     ,  perStakeholder :: !StakeholderId
                     ,  perDecision    :: !Bool}
    | PollWrongHeaderBlockVersion { pwhpvGiven   :: !BlockVersion
                                  , pwhpvAdopted :: !BlockVersion}
    | PollBadBlockVersion { pbpvUpId       :: !UpId
                            ,  pbpvGiven   :: !BlockVersion
                            ,  pbpvAdopted :: !BlockVersion}
    | PollTooLargeProposal { ptlpUpId  :: !UpId
                           , ptlpSize  :: !Byte
                           , ptlpLimit :: !Byte
                           }
    | PollMoreThanOneProposalPerEpoch { ptopFrom :: !StakeholderId
                                      , ptopUpId :: !UpId
                                      }
    | PollUnknownAttributesInProposal { puapUpId  :: !UpId
                                      , puapAttrs :: !UpAttributes
                                      }
    | PollTipMismatch { ptmTipDB     :: !HeaderHash
                      , ptmTipMemory :: !HeaderHash
                      }
    | PollInvalidUpdatePayload !Text
    | PollInternalError !Text

instance Buildable PollVerFailure where
    build (PollInconsistentBVM {..}) =
        bprint ("proposal "%shortHashF%" contains block version"%
                " which is already competing and its"%
                " BlockVersionModifier is different"%
                " (expected "%F.build%", proposed "%F.build%")")
        pibUpId pibExpected pibFound
    build (PollAlreadyAdoptedDiffers {..}) =
        bprint ("proposal "%shortHashF%" contains block version"%
                " which is already adopted and its"%
                " BlockVersionModifier doesn't correspond to the adopted"%
                " BlockVersionData (adopted "%F.build%", proposed "%F.build%")")
        paadUpId paadAdopted paadProposed
    build (PollWrongScriptVersion {..}) =
        bprint ("proposal "%shortHashF%" contains script version"%
                " which is neither same not greater by one than the"%
                " adopted one (adopted one is "%int%
                ", proposed one is "%int%")")
        pwsvUpId pwsvAdopted pwsvProposed
    build (PollLargeMaxBlockSize maxPossible found upId) =
        bprint ("proposal "%F.build%" tried to increase max block size"%
                " beyond what is allowed"%
                " (expected max. "%memory%", found "%memory%")")
        upId maxPossible found
    build (PollBootstrapEraInvalidChange last adopted proposed upId) =
        bprint ("proposal "%F.build%" tried to change the end of the bootstrap"%
                " era to epoch"%F.build%", but the bootstrap era has ended with"%
                " unlock stakes epoch "%F.build%", and now the epoch is "%
               F.build%".")
        upId proposed adopted last
    build (PollProposalAlreadyActive upId) =
        bprint ("proposal "%F.build%" was already proposed") upId
    build (PollNotFoundScriptVersion pv) =
        bprint ("not found script version for protocol version "%F.build) pv
    build (PollSmallProposalStake threshold actual upId) =
        bprint ("proposal "%F.build%
                " doesn't have enough stake from positive votes "%
                "(threshold is "%coinF%", proposal has "%coinF%")")
        upId threshold actual
    build (PollNotRichman id threshold stake) =
        bprint ("voter "%F.build%" is not richman (his stake is "%stext%", but"%
                " threshold is "%coinF%")")
        id (maybe "negligible" (sformat coinF) stake) threshold
    build (PollUnknownProposal stakeholder proposal) =
        bprint (F.build%" has voted for unkown proposal "%F.build)
        stakeholder proposal
    build (PollUnknownStakes epoch) =
        bprint ("stake distribution for epoch "%F.build%" is unknown") epoch
    build (PollWrongSoftwareVersion {..}) =
        bprint ("proposal "%F.build%" has wrong software version for app "%
               F.build%" (last known is "%stext%", proposal contains "%int%")")
        pwsvUpId pwsvApp (maybe "unknown" pretty pwsvStored) pwsvGiven
    build (PollProposalIsDecided {..}) =
        bprint ("proposal "%F.build%" is in decided state, but stakeholder "%
               F.build%" has voted for it")
        ppidUpId ppidStakeholder
    build (PollExtraRevote {..}) =
        bprint ("stakeholder "%F.build%" vote "%stext%" proposal "
                %F.build%" more than once")
        perStakeholder (bool "against" "for" perDecision) perUpId
    build (PollWrongHeaderBlockVersion {..}) =
        bprint ("wrong protocol version has been seen in header: "%
               F.build%" (current adopted is "%F.build%"), "%
                "this version is smaller than last adopted "%
                "or is not confirmed")
        pwhpvGiven pwhpvAdopted
    build (PollBadBlockVersion {..}) =
        bprint ("proposal "%F.build%" has bad protocol version: "%
               F.build%" (current adopted is "%F.build%")")
        pbpvUpId pbpvGiven pbpvAdopted
    build (PollTooLargeProposal {..}) =
        bprint ("update proposal "%shortHashF%" exceeds maximal size ("%
                asInt%" > "%asInt%")")
        ptlpUpId ptlpSize ptlpLimit
    build (PollMoreThanOneProposalPerEpoch {..}) =
        bprint ("stakeholder "%shortHashF%
                " proposed second proposal "%shortHashF%" in epoch")
        ptopFrom ptopUpId
    build (PollUnknownAttributesInProposal {..}) =
        bprint ("proposal "%shortHashF%" has unknown attributes "%F.build)
        puapUpId puapAttrs
    build (PollTipMismatch {..}) =
        bprint ("tip we store in US mem-state ("%shortHashF%
                ") differs from the tip we store in DB ("%F.build%")")
        ptmTipMemory ptmTipDB
    build (PollInvalidUpdatePayload msg) =
        bprint ("invalid update payload: "%stext) msg
    build (PollInternalError msg) =
        bprint ("internal error: "%stext) msg

-- | Report an error if it's unexpected.
--
-- If tips are different, we report error, because it's suspicious and
-- we want to review logs. If it's internal error, we definitely want
-- to investigate it.
reportUnexpectedError
    :: ( Monad m, MonadReporting m )
    => m (Either PollVerFailure a)
    -> m (Either PollVerFailure a)
reportUnexpectedError action = do
    res <- action
    -- REPORT:ERROR Internal error in update system or tips mismatch.
    res <$
        case res of
            Left (PollInternalError msg) ->
                reportError $
                "Internal error occurred in update system: " <> msg
            Left (err@(PollTipMismatch {})) -> reportError (pretty err)
            _ -> pass
