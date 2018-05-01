{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Toil failures.

module Pos.Txp.Toil.Failure
       ( ToilVerFailure (..)
       , WitnessVerFailure (..)
       , TxOutVerFailure (..)
       ) where

import           Universum

import           Formatting (bprint, int, ords, shown, stext, (%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (build))
import           GHC.TypeLits (TypeError)
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util (listJson)

import           Pos.Core (Address, HeaderHash, ScriptVersion, TxFeePolicy,
                     addressDetailedF, addressF)
import           Pos.Core.Txp (TxIn, TxInWitness, TxOut (..))
import           Pos.Data.Attributes (UnparsedFields)
import           Pos.Script (PlutusError)
import           Pos.Txp.Toil.Types (TxFee)
import           Pos.Util (DisallowException)

----------------------------------------------------------------------------
-- ToilVerFailure
----------------------------------------------------------------------------

-- | Result of transaction processing
data ToilVerFailure
    = ToilKnown -- ^ Transaction is already in the storage (cache)
    -- | ToilTipsMismatch oldTip newTip
    | ToilTipsMismatch !HeaderHash !HeaderHash
    | ToilSlotUnknown
    | ToilOverwhelmed !Int -- ^ Local transaction storage is full --
                            -- can't accept more txs. Current limit is attached.
    | ToilNotUnspent !TxIn -- ^ Tx input is not a known unspent input.
    -- | ToilOutGreaterThanIn inputSum outputSum
    | ToilOutGreaterThanIn !Integer !Integer
    | ToilInconsistentTxAux !Text
    | ToilInvalidOutput !Word32 !TxOutVerFailure
    | ToilUnknownInput !Word32 !TxIn
    -- | The witness can't be used to justify spending an output – either
    --     * it has a wrong type, e.g. PKWitness for a script address, or
    --     * it has the right type but doesn't match the address, e.g. the
    --       hash of key in PKWitness is not equal to the address.
    | ToilWitnessDoesntMatch !Word32 !TxIn !TxOut !TxInWitness
    -- | The witness could in theory justify spending an output, but it
    -- simply isn't valid (the signature doesn't pass validation, the
    -- validator–redeemer pair produces 'False' when executed, etc).
    | ToilInvalidWitness !Word32 !TxInWitness !WitnessVerFailure
    -- | ToilTooLargeTx acutalSize limit
    | ToilTooLargeTx !Byte !Byte
    | ToilInvalidMinFee !TxFeePolicy !Text !Byte
    -- | ToilInsufficientFee policy actualFee minFee size
    | ToilInsufficientFee !TxFeePolicy !TxFee !TxFee !Byte
    | ToilUnknownAttributes !UnparsedFields
    | ToilNonBootstrapDistr !(NonEmpty Address)
    | ToilRepeatedInput
    | ToilEmptyAfterFilter
    deriving (Show, Eq)

instance TypeError (DisallowException ToilVerFailure) =>
         Exception ToilVerFailure

instance Buildable ToilVerFailure where
    build ToilKnown =
        "transaction already is in the mem pool"
    build (ToilTipsMismatch dbTip localTip) =
        bprint ("Something is bad with this node, tips mismatch, "%
                "tip from DB is "%F.build%", local tip is "%F.build)
        dbTip localTip
    build ToilSlotUnknown =
        "can't process, current slot is unknown"
    build (ToilOverwhelmed limit) =
        bprint ("max size of the mem pool is reached which is "%shown) limit
    build (ToilNotUnspent txId) =
        bprint ("input is not a known unspent input: "%F.build) txId
    build (ToilOutGreaterThanIn tInputSum tOutputSum) =
        bprint ("sum of outputs is greater than sum of inputs ("%int%" < "%int%")")
        tInputSum tOutputSum
    build (ToilInconsistentTxAux msg) =
        bprint ("TxAux is inconsistent: "%stext) msg
    build (ToilInvalidOutput n reason) =
        bprint (ords%" output is invalid:\n'"%
                " reason: "%F.build)
            n reason
    build (ToilWitnessDoesntMatch i txIn txOut@TxOut {..} witness) =
        bprint ("input #"%int%"'s witness doesn't match address "%
                "of corresponding output:\n"%
                "  input: "%F.build%"\n"%
                "  output spent by this input: "%F.build%"\n"%
                "  address details: "%addressDetailedF%"\n"%
                "  witness: "%F.build)
            i txIn txOut txOutAddress witness
    build (ToilInvalidWitness i witness reason) =
        bprint ("input #"%int%"'s witness doesn't pass verification:\n"%
                "  witness: "%F.build%"\n"%
                "  reason: "%F.build)
            i witness reason
    build (ToilTooLargeTx ttltSize ttltLimit) =
        bprint ("transaction's size exceeds limit "%
                "("%memory%" > "%memory%")") ttltSize ttltLimit
    build (ToilInvalidMinFee timfPolicy timfReason timfSize) =
        bprint (F.build%" generates invalid minimal fee on a "%
                "transaction of size "%memory%", reason: "%stext)
            timfPolicy
            timfSize
            timfReason
    build (ToilInsufficientFee tifPolicy tifFee tifMinFee tifSize) =
        bprint ("transaction of size "%memory%" does not adhere to "%
               F.build%"; it has fee "%F.build%" but needs "%F.build)
            tifSize
            tifPolicy
            tifFee
            tifMinFee
    build (ToilUnknownAttributes uf) =
        bprint ("transaction has unknown attributes: "%shown) uf
    build (ToilNonBootstrapDistr addresses) =
        bprint ("we are in bootstrap era, but some addresses have distribution"%
                " which is not 'BootstrapEraDistr': "%listJson) addresses
    build ToilRepeatedInput =
        "transaction tries to spend an unspent input more than once"
    build (ToilUnknownInput inpId txIn) =
       bprint ("vtcVerifyAllIsKnown is True, but the input #"%int%" "%F.build%" is unknown") inpId txIn

    build ToilEmptyAfterFilter =
       "transaction list is empty after filtering out asset-locked source addresses"

----------------------------------------------------------------------------
-- WitnessVerFailure
----------------------------------------------------------------------------

-- | Result of checking a witness.
data WitnessVerFailure
    -- | The signature of a 'PKWitness' doesn't pass validation
    = WitnessWrongSignature
    -- | Validator and redeemer script versions don't match
    | WitnessScriptVerMismatch ScriptVersion ScriptVersion
    -- | Don't know how to handle script version
    | WitnessUnknownScriptVer ScriptVersion
    -- | Plutus error (e.g. exhausted execution steps, redeemer script
    -- returning 'False', etc)
    | WitnessScriptError PlutusError
    -- | Don't know how to handle this witness type
    | WitnessUnknownType Word8
    deriving (Show, Eq, Generic, NFData)

instance Buildable WitnessVerFailure where
    build WitnessWrongSignature =
        bprint "the signature in the witness doesn't pass validation"
    build (WitnessScriptVerMismatch val red) =
        bprint ("validator and redeemer script versions don't match: "%
                "validator version = "%F.build%", script version = "%F.build) val red
    build (WitnessUnknownScriptVer ver) =
        bprint ("unknown/unhandleable script version: "%F.build) ver
    build (WitnessScriptError err) =
        bprint ("error when executing scripts: "%F.build) err
    build (WitnessUnknownType t) =
        bprint ("unknown witness type: "%F.build) t

----------------------------------------------------------------------------
-- TxOutVerFailure
----------------------------------------------------------------------------

-- | Result of checking transaction output.
data TxOutVerFailure
    -- | Not all attributes for the output are known
    = TxOutUnknownAttributes Address
    -- | Can't send to an address with unknown type
    | TxOutUnknownAddressType Address
    -- | Can't send to a redeem address
    | TxOutRedeemAddressProhibited Address
    deriving (Show, Eq, Generic, NFData)

instance Buildable TxOutVerFailure where
    build (TxOutUnknownAttributes addr) =
        bprint ("address "%addressF%" has unknown attributes") addr
    build (TxOutUnknownAddressType addr) =
        bprint ("sends money to an addresss with unknown type ("
                %addressF%"), this is prohibited") addr
    build (TxOutRedeemAddressProhibited addr) =
        bprint ("sends money to a redeem address ("
                %addressF%"), this is prohibited") addr
