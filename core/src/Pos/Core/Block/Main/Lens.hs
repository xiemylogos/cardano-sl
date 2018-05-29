{-# LANGUAGE TypeOperators #-}

-- | Lenses for main blockchain types.
--
-- Lenses whose name starts with `mainBlock' are from 'MainBlock' to
-- small parts of it. It makes it clear what exactly is stored in
-- 'MainBlock'. Similar fact is true for `mainHeader' prefix.

module Pos.Core.Block.Main.Lens
       (
         -- * MainToSign
         msHeaderHash
       , msBodyProof
       , msSlot
       , msChainDiff
       , msExtraHeader

         -- * Extra types
       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mebAttributes

         -- * MainConsensusData
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature

         -- * MainBlockHeader
       , mainHeaderPrevBlock
       , mainHeaderProof
       , mainHeaderSlot
       , mainHeaderLeaderKey
       , mainHeaderDifficulty
       , mainHeaderSignature
       , mainHeaderBlockVersion
       , mainHeaderSoftwareVersion
       , mainHeaderAttributes

         -- * MainBody
       , mbSscPayload
       , mbTxPayload
       , mbDlgPayload
       , mbUpdatePayload
       , mbTxs
       , mbWitnesses

         -- * MainBlock
       , mainBlockPrevBlock
       , mainBlockProof
       , mainBlockSlot
       , mainBlockLeaderKey
       , mainBlockDifficulty
       , mainBlockSignature
       , mainBlockBlockVersion
       , mainBlockSoftwareVersion
       , mainBlockHeaderAttributes
       , mainBlockEBDataProof
       , mainBlockTxPayload
       , mainBlockSscPayload
       , mainBlockDlgPayload
       , mainBlockUpdatePayload
       , mainBlockAttributes
       ) where

import           Universum

import           Control.Lens (makeLenses)

import           Pos.Core.Block.Blockchain (gbBody, gbExtra, gbHeader, gbPrevBlock, gbhBodyProof,
                                            gbhConsensus, gbhExtra, gbhPrevBlock)
import           Pos.Core.Block.Main.Types (BlockBodyAttributes, BlockHeaderAttributes,
                                            MainBody (..), MainExtraBodyData, MainExtraHeaderData,
                                            MainProof (..))
import           Pos.Core.Block.Union.Types (BlockSignature, HeaderHash, MainBlock, MainBlockHeader,
                                             MainConsensusData (..), MainToSign (..))
import           Pos.Core.Common (ChainDifficulty)
import           Pos.Core.Delegation (DlgPayload)
import           Pos.Core.Slotting (SlotId)
import           Pos.Core.Ssc (SscPayload)
import           Pos.Core.Txp (Tx, TxPayload, TxWitness, txpTxs, txpWitnesses)
import           Pos.Core.Update (BlockVersion, SoftwareVersion, UpdatePayload)
import           Pos.Crypto (Hash, PublicKey)

----------------------------------------------------------------------------
-- MainToSign
----------------------------------------------------------------------------

makeLenses ''MainToSign

----------------------------------------------------------------------------
-- Extra types
----------------------------------------------------------------------------

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData

----------------------------------------------------------------------------
-- MainConsensusData
----------------------------------------------------------------------------

makeLenses 'MainConsensusData

----------------------------------------------------------------------------
-- MainBlockHeader
----------------------------------------------------------------------------

-- | Lens from 'MainBlockHeader' to 'HeaderHash' of its parent.
mainHeaderPrevBlock :: Lens' (MainBlockHeader attr) HeaderHash
mainHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'MainBlockHeader' to 'MainProof'.
mainHeaderProof :: Lens' (MainBlockHeader attr) MainProof
mainHeaderProof = gbhBodyProof

-- | Lens from 'MainBlockHeader' to 'SlotId'.
mainHeaderSlot :: Lens' (MainBlockHeader attr) SlotId
mainHeaderSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlockHeader' to 'PublicKey'.
mainHeaderLeaderKey :: Lens' (MainBlockHeader attr) PublicKey
mainHeaderLeaderKey = gbhConsensus . mcdLeaderKey

-- | Lens from 'MainBlockHeader' to 'ChainDifficulty'.
mainHeaderDifficulty :: Lens' (MainBlockHeader attr) ChainDifficulty
mainHeaderDifficulty = gbhConsensus . mcdDifficulty

-- | Lens from 'MainBlockHeader' to 'Signature'.
mainHeaderSignature :: Lens' (MainBlockHeader attr) BlockSignature
mainHeaderSignature = gbhConsensus . mcdSignature

-- | Lens from 'MainBlockHeader' to 'BlockVersion'.
mainHeaderBlockVersion :: Lens' (MainBlockHeader attr) BlockVersion
mainHeaderBlockVersion = gbhExtra . mehBlockVersion

-- | Lens from 'MainBlockHeader' to 'SoftwareVersion'.
mainHeaderSoftwareVersion :: Lens' (MainBlockHeader attr) SoftwareVersion
mainHeaderSoftwareVersion = gbhExtra . mehSoftwareVersion

-- | Lens from 'MainBlockHeader' to 'BlockHeaderAttributes'.
mainHeaderAttributes :: Lens' (MainBlockHeader attr) BlockHeaderAttributes
mainHeaderAttributes = gbhExtra . mehAttributes

-- | Lens from 'MainBlockHeader' to 'MainExtraBodyData'
mainHeaderEBDataProof :: Lens' (MainBlockHeader attr) (Hash MainExtraBodyData)
mainHeaderEBDataProof = gbhExtra . mehEBDataProof

----------------------------------------------------------------------------
-- MainBody
----------------------------------------------------------------------------

makeLenses 'MainBody

-- | Lens for transaction tree in main block body.
mbTxs :: Lens' MainBody ([Tx])
mbTxs = mbTxPayload . txpTxs

-- | Lens for witness list in main block body.
mbWitnesses :: Lens' MainBody [TxWitness]
mbWitnesses = mbTxPayload . txpWitnesses

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Lens from 'MainBlock' to 'HeaderHash' of its parent.
mainBlockPrevBlock :: Lens' (MainBlock attr) HeaderHash
mainBlockPrevBlock = gbPrevBlock

-- | Lens from 'MainBlock' to 'MainProof'.
mainBlockProof :: Lens' (MainBlock attr) MainProof
mainBlockProof = gbHeader . mainHeaderProof

-- | Lens from 'MainBlock' to 'SlotId'.
mainBlockSlot :: Lens' (MainBlock attr) SlotId
mainBlockSlot = gbHeader . mainHeaderSlot

-- | Lens from 'MainBlock' to 'PublicKey'.
mainBlockLeaderKey :: Lens' (MainBlock attr) PublicKey
mainBlockLeaderKey = gbHeader . mainHeaderLeaderKey

-- | Lens from 'MainBlock' to 'ChainDifficulty'.
mainBlockDifficulty :: Lens' (MainBlock attr) ChainDifficulty
mainBlockDifficulty = gbHeader . mainHeaderDifficulty

-- | Lens from 'MainBlock' to 'Signature'.
mainBlockSignature :: Lens' (MainBlock attr) BlockSignature
mainBlockSignature = gbHeader . mainHeaderSignature

-- | Lens from 'MainBlock' to 'BlockVersion'.
mainBlockBlockVersion :: Lens' (MainBlock attr) BlockVersion
mainBlockBlockVersion = gbHeader . mainHeaderBlockVersion

-- | Lens from 'MainBlock' to 'SoftwareVersion'.
mainBlockSoftwareVersion :: Lens' (MainBlock attr) SoftwareVersion
mainBlockSoftwareVersion = gbHeader . mainHeaderSoftwareVersion

-- | Lens from 'MainBlock' to 'BlockHeaderAttributes'.
mainBlockHeaderAttributes :: Lens' (MainBlock attr) BlockHeaderAttributes
mainBlockHeaderAttributes = gbHeader . mainHeaderAttributes

-- | Lens from 'MainBlock' to proof (hash) of 'MainExtraBodyData'.
mainBlockEBDataProof :: Lens' (MainBlock attr) (Hash MainExtraBodyData)
mainBlockEBDataProof = gbHeader . mainHeaderEBDataProof

-- | Lens from 'MainBlock' to 'TxPayload'.
mainBlockTxPayload :: Lens' (MainBlock attr) TxPayload
mainBlockTxPayload = gbBody . mbTxPayload

-- | Lens from 'MainBlock' to 'SscPayload'.
mainBlockSscPayload :: Lens' (MainBlock attr) SscPayload
mainBlockSscPayload = gbBody . mbSscPayload

-- | Lens from 'MainBlock' to 'UpdatePayload'.
mainBlockUpdatePayload :: Lens' (MainBlock attr) UpdatePayload
mainBlockUpdatePayload = gbBody . mbUpdatePayload

-- | Lens from 'MainBlock' to 'DlgPayload'.
mainBlockDlgPayload :: Lens' (MainBlock attr) DlgPayload
mainBlockDlgPayload = gbBody . mbDlgPayload

-- | Lens from 'MainBlock' to 'BlockBodyAttributes'.
mainBlockAttributes :: Lens' (MainBlock attr) BlockBodyAttributes
mainBlockAttributes = gbExtra . mebAttributes
