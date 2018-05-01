module Pos.Core.Update.BlockVersionData
       ( BlockVersionData (..)
       , isBootstrapEraBVD
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Time.Units (Millisecond)
import           Formatting (bprint, int, (%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (build))
import           Serokell.Data.Memory.Units (Byte, memory)

import           Pos.Core.Common (CoinPortion, ScriptVersion, TxFeePolicy)
import           Pos.Core.Slotting (EpochIndex, FlatSlotId, isBootstrapEra)
import           Pos.Util.Orphans ()

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Binary ()
import           Pos.Core.Update.SoftforkRule

-- | Data which is associated with 'BlockVersion'.
data BlockVersionData = BlockVersionData
    { bvdScriptVersion     :: !ScriptVersion
    , bvdSlotDuration      :: !Millisecond
    , bvdMaxBlockSize      :: !Byte
    , bvdMaxHeaderSize     :: !Byte
    , bvdMaxTxSize         :: !Byte
    , bvdMaxProposalSize   :: !Byte
    , bvdMpcThd            :: !CoinPortion
    , bvdHeavyDelThd       :: !CoinPortion
    , bvdUpdateVoteThd     :: !CoinPortion
    , bvdUpdateProposalThd :: !CoinPortion
    , bvdUpdateImplicit    :: !FlatSlotId
    , bvdSoftforkRule      :: !SoftforkRule
    , bvdTxFeePolicy       :: !TxFeePolicy
    , bvdUnlockStakeEpoch  :: !EpochIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData BlockVersionData where

instance Buildable BlockVersionData where
    build BlockVersionData {..} =
      bprint ("{ script version: "%F.build%
              ", slot duration: "%int%" mcs"%
              ", block size limit: "%memory%
              ", header size limit: "%memory%
              ", tx size limit: "%memory%
              ", proposal size limit: "%memory%
              ", mpc threshold: "%F.build%
              ", heavyweight delegation threshold: "%F.build%
              ", update vote threshold: "%F.build%
              ", update proposal threshold: "%F.build%
              ", update implicit period: "%int%" slots"%
              ", softfork rule: "%F.build%
              ", tx fee policy: "%F.build%
              ", unlock stake epoch: "%F.build%
              " }")
        bvdScriptVersion
        bvdSlotDuration
        bvdMaxBlockSize
        bvdMaxHeaderSize
        bvdMaxTxSize
        bvdMaxProposalSize
        bvdMpcThd
        bvdHeavyDelThd
        bvdUpdateVoteThd
        bvdUpdateProposalThd
        bvdUpdateImplicit
        bvdSoftforkRule
        bvdTxFeePolicy
        bvdUnlockStakeEpoch

-- | Version of 'isBootstrapEra' which takes 'BlockVersionData'
-- instead of unlock stake epoch.
isBootstrapEraBVD :: BlockVersionData -> EpochIndex -> Bool
isBootstrapEraBVD adoptedBVD = isBootstrapEra (bvdUnlockStakeEpoch adoptedBVD)

deriveSimpleBi ''BlockVersionData [
    Cons 'BlockVersionData [
        Field [| bvdScriptVersion     :: ScriptVersion |],
        Field [| bvdSlotDuration      :: Millisecond   |],
        Field [| bvdMaxBlockSize      :: Byte          |],
        Field [| bvdMaxHeaderSize     :: Byte          |],
        Field [| bvdMaxTxSize         :: Byte          |],
        Field [| bvdMaxProposalSize   :: Byte          |],
        Field [| bvdMpcThd            :: CoinPortion   |],
        Field [| bvdHeavyDelThd       :: CoinPortion   |],
        Field [| bvdUpdateVoteThd     :: CoinPortion   |],
        Field [| bvdUpdateProposalThd :: CoinPortion   |],
        Field [| bvdUpdateImplicit    :: FlatSlotId    |],
        Field [| bvdSoftforkRule      :: SoftforkRule  |],
        Field [| bvdTxFeePolicy       :: TxFeePolicy   |],
        Field [| bvdUnlockStakeEpoch  :: EpochIndex    |]
    ]]

deriveSafeCopySimple 0 'base ''BlockVersionData
