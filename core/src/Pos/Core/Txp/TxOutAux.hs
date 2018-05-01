module Pos.Core.Txp.TxOutAux
       ( TxOutAux (..)
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, (%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (build))

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Txp.Tx (TxOut)

-- | Transaction output and auxilary data corresponding to it.
-- [CSL-366] Add more data.
data TxOutAux = TxOutAux
    { toaOut   :: !TxOut
    -- ^ Tx output
    } deriving (Generic, Show, Eq, Ord)

instance Buildable TxOutAux where
    build (TxOutAux out) = bprint ("{txout = "%F.build%"}") out

instance NFData TxOutAux

deriveSimpleBi ''TxOutAux [
    Cons 'TxOutAux [
        Field [| toaOut :: TxOut |]
    ]]

deriveSafeCopySimple 0 'base ''TxOutAux
