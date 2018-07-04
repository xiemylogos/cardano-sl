{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( BlockTxpGenMode
       , genBlockNoApply
       , genBlocksNoApply
       , genBlocks
       ) where

import           Universum

import           Control.Lens (at, ix, _Wrapped)
import           Control.Monad.Random.Strict (RandT, mapRandT)
import           Data.Default (Default)
import           Formatting (build, sformat, shown, (%))
import           System.Random (RandomGen (..))
import           System.Wlog (logWarning)

import           Pos.AllSecrets (HasAllSecrets (..), unInvSecretsMap)
import           Pos.Block.Logic (VerifyBlocksContext (..), applyBlocksUnsafe,
                     createMainBlockInternal, getVerifyBlocksContext,
                     getVerifyBlocksContext', normalizeMempool,
                     verifyBlocksPrefix)
import           Pos.Block.Lrc (lrcSingleShot)
import           Pos.Block.Slog (ShouldCallBListener (..))
import           Pos.Block.Types (Blund)
import           Pos.Communication.Message ()
import           Pos.Core (EpochOrSlot (..), HasHeaderHash (..), SlotId (..),
                     addressHash, epochIndexL, getEpochOrSlot, getSlotIndex)
import           Pos.Core.Block (Block, BlockHeader, getBlockHeader)
import           Pos.Core.Block.Constructors (mkGenesisBlock)
import           Pos.Crypto (ProtocolMagic, pskDelegatePk)
import qualified Pos.DB.BlockIndex as DB
import           Pos.Delegation.Logic (getDlgTransPsk)
import           Pos.Delegation.Types (ProxySKBlockInfo)
import           Pos.Generator.Block.Error (BlockGenError (..))
import           Pos.Generator.Block.Mode (BlockGenMode, BlockGenRandMode,
                     MonadBlockGen, MonadBlockGenInit, mkBlockGenContext,
                     usingPrimaryKey, withCurrentSlot)
import           Pos.Generator.Block.Param (BlockGenParams,
                     HasBlockGenParams (..))
import           Pos.Generator.Block.Payload (genPayload)
import           Pos.Lrc.Context (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB as LrcDB
import           Pos.Txp (MempoolExt, MonadTxpLocal, TxpGlobalSettings)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Util (HasLens', maybeThrow, _neHead)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

type BlockTxpGenMode g ctx m =
    ( RandomGen g
    , MonadBlockGenInit ctx m
    , HasLens' ctx TxpGlobalSettings
    , Default (MempoolExt m)
    , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
    )

foldM' :: forall a t m. Monad m => (a -> t -> m a) -> a -> [t] -> m a
foldM' combine = go
    where
    go !base []     = return base
    go !base (x:xs) = combine base x >>= flip go xs

-- | Common code used by @'genBlocksNoApply'@ and @'genBlocks@.  The former
-- generates a non empty list of @'Block'@s (using @'genBlockNoApply'@), the
-- latter also applies them one by one (using @'genBlock'@) an returns a list of
-- @'Blund'@s.
genBlocks'
    :: forall g ctx m a t .
       ( BlockTxpGenMode g ctx m
       , Semigroup t
       , Monoid t
       )
    => BlockGenParams
    -> (a -> Block)  -- ^ get @'Block'@ out of generated value
    -> (    EpochOrSlot
         -> BlockHeader
         -> BlockGenRandMode (MempoolExt m) g m (Maybe a)
       )
       -- ^ generator of @'Block'@ or @'Blund'@
    -> (Maybe a -> t)
    -> RandT g m t
genBlocks' params getBlock gen inj = do
    ctx <- lift $ mkBlockGenContext @(MempoolExt m) params
    mapRandT (`runReaderT` ctx) genBlocksDo
  where
    genBlocksDo :: RandT g (BlockGenMode (MempoolExt m) m) t
    genBlocksDo = do
        let numberOfBlocks = params ^. bgpBlockCount
        tipHeader <- lift DB.getTipHeader
        let tipEOS = getEpochOrSlot tipHeader
        let startEOS = succ tipEOS
        let finishEOS = toEnum $ fromEnum tipEOS + fromIntegral numberOfBlocks
        snd <$> foldM' genOne (tipHeader, mempty) [startEOS .. finishEOS]

    genOne
        :: (BlockHeader, t)
        -> EpochOrSlot
        -> RandT g (BlockGenMode (MempoolExt m) m) (BlockHeader, t)
    genOne (header, t) eos = do
        gen eos header >>= \case
            Nothing -> error $ sformat ("genBlocks': failed to generate a block: previous hash: "%shown) (headerHash header)
            Just a -> return (getBlockHeader $ getBlock a, (t <>) . inj $ Just a)

-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
-- The blocks themselves can be combined and retained according to some monoid.
-- Intermediate results will be forced. Blocks can be generated, written to
-- disk, then collected by using '()' as the monoid and 'const ()' as the
-- injector, for example.
genBlocks ::
       forall g ctx m t . (HasTxpConfiguration, BlockTxpGenMode g ctx m, Semigroup t, Monoid t)
    => ProtocolMagic
    -> BlockGenParams
    -> (Maybe Blund -> t)
    -> RandT g m t
genBlocks pm params inj = genBlocks' params fst (\epoch _ -> genBlock pm epoch) inj

-- | Generate an arbitrary sequence of blocks without applying them.
genBlocksNoApply ::
       forall g ctx m t . (HasTxpConfiguration, BlockTxpGenMode g ctx m, Semigroup t, Monoid t)
    => ProtocolMagic
    -> BlockGenParams
    -> (Maybe Block -> t)
    -> RandT g m t
genBlocksNoApply pm params inj = genBlocks' params identity (genBlockNoApply pm) inj

-- | Generate a 'Block' for the given epoch or slot (geneis block in the formet
-- case and main block in the latter case) and do not apply it.
genBlockNoApply
    :: forall g ctx m.
       ( RandomGen g
       , MonadBlockGen ctx m
       , Default (MempoolExt m)
       , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
       , HasTxpConfiguration
       )
    => ProtocolMagic
    -> EpochOrSlot
    -> BlockHeader -- ^ previoud block header
    -> BlockGenRandMode (MempoolExt m) g m (Maybe Block)
genBlockNoApply pm eos header = do
    let epoch = eos ^. epochIndexL
    lift $ unlessM ((epoch ==) <$> LrcDB.getEpoch) (lrcSingleShot pm epoch)
    -- We need to know leaders to create any block.
    leaders <- lift $ lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeadersForEpoch
    case eos of
        EpochOrSlot (Left _) -> do
            let genesisBlock = mkGenesisBlock pm (Right header) epoch leaders
            return $ Just $ Left genesisBlock
        EpochOrSlot (Right slot@SlotId {..}) -> withCurrentSlot slot $ do
            genPayload pm slot
            leader <-
                lift $ maybeThrow
                    (BGInternal "no leader")
                    (leaders ^? ix (fromIntegral $ getSlotIndex siSlot))
            secrets <-
                unInvSecretsMap . view asSecretKeys <$> view blockGenParams
            transCert <- lift $ getDlgTransPsk leader
            let creator = maybe leader (addressHash . pskDelegatePk . snd) transCert
            let maybeLeader = secrets ^. at creator
            canSkip <- view bgpSkipNoKey
            case (maybeLeader, canSkip) of
                (Nothing,True)     -> do
                    lift $ logWarning $
                        sformat ("Skipping block creation for leader "%build%
                                 " as no related key was found")
                                leader
                    pure Nothing
                (Nothing,False)    ->
                    throwM $ BGUnknownSecret leader
                (Just leaderSK, _) ->
                    -- When we know the secret key we can proceed to the actual creation.
                    Just <$> usingPrimaryKey leaderSK
                             (lift $ genMainBlock slot (swap <$> transCert))
    where
    genMainBlock ::
        SlotId ->
        ProxySKBlockInfo ->
        BlockGenMode (MempoolExt m) m Block
    genMainBlock slot proxySkInfo =
        createMainBlockInternal pm slot proxySkInfo header >>= \case
            Left err -> throwM (BGFailedToCreate err)
            Right mainBlock -> return $ Right mainBlock

-- | Generate a valid 'Block' for the given epoch or slot (genesis block
-- in the former case and main block the latter case) and apply it.
genBlock ::
       forall g ctx m.
       ( RandomGen g
       , MonadBlockGen ctx m
       , Default (MempoolExt m)
       , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
       , HasTxpConfiguration
       )
    => ProtocolMagic
    -> EpochOrSlot
    -> BlockGenRandMode (MempoolExt m) g m (Maybe Blund)
genBlock pm eos = do
    let epoch = eos ^. epochIndexL
    tipHeader <- lift DB.getTipHeader
    genBlockNoApply pm eos tipHeader >>= \case
        Just block@Left{}   -> do
            let slot0 = SlotId epoch minBound
            ctx <- getVerifyBlocksContext' (Just slot0)
            fmap Just $ withCurrentSlot slot0 $ lift $ verifyAndApply ctx block
        Just block@Right {} -> do
            ctx <- getVerifyBlocksContext
            fmap Just $ lift $ verifyAndApply ctx block
        Nothing -> return Nothing
    where
    verifyAndApply
        :: VerifyBlocksContext
        -> Block
        -> BlockGenMode (MempoolExt m) m Blund
    verifyAndApply ctx block =
        verifyBlocksPrefix pm ctx (one block) >>= \case
            Left err -> throwM (BGCreatedInvalid err)
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                    blund = (block, undo)
                applyBlocksUnsafe pm
                    (vbcBlockVersion ctx)
                    (vbcBlockVersionData ctx)
                    (ShouldCallBListener True)
                    (one blund)
                    (Just pollModifier)
                normalizeMempool pm
                pure blund
