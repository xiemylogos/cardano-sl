module Pos.Core.Slotting.SlotCount
       ( SlotCount (..)
       ) where

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting.Buildable (Buildable)
import           System.Random (Random (..))
import           Universum

import           Pos.Binary.Class (Bi (..))

newtype SlotCount = SlotCount {getSlotCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

instance Bi SlotCount where
    encode = encode . getSlotCount
    decode = SlotCount <$> decode

deriveSafeCopySimple 0 'base ''SlotCount
