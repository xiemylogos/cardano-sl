module Bench.Pos.Criterion.Block.Logic
    ( runBenchmark
    ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT, mapRandT)
import           Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, defaultConfig, defaultMainWith, env, nf, nfIO)
import           Criterion.Types (Config (..), Verbosity (..))
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (convertUnit)
import           System.Random (newStdGen)
import           System.Wlog (LoggerName (..))
import           Serokell.Util.Verify (isVerSuccess)

import           Mockable.CurrentTime (realTime)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Block.Logic.VAR (VerifyBlocksContext, verifyAndApplyBlocks, verifyBlocksPrefix, getVerifyBlocksContext', rollbackBlocks)
import           Pos.Block.Logic.Integrity (VerifyHeaderParams (..), verifyHeader)
import           Pos.Core (Block, BlockHeader, EpochOrSlot (..), SlotId, getBlockHeader)
import           Pos.Core.Chrono (OldestFirst (..), NE, nonEmptyNewestFirst)
import           Pos.Core.Common (BlockCount (..), SlotLeaders, unsafeCoinPortionFromDouble)
import           Pos.Core.Configuration (genesisBlockVersionData, genesisData, genesisSecretKeys, slotSecurityParam)
import           Pos.Core.Genesis (FakeAvvmOptions (..), GenesisData (..), GenesisInitializer (..), TestnetBalanceOptions (..))
import           Pos.Core.Slotting (Timestamp (..), epochIndexL, getEpochOrSlot)
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB (getTipHeader)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..), genBlocksNoApply, genBlockNoApply, mkBlockGenContext)
import           Pos.Lrc.Context (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB as LrcDB
import           Pos.Launcher.Configuration (ConfigurationOptions (..), HasConfigurations, defaultConfigurationOptions, withConfigurationsM)
import           Pos.Txp.Logic.Global (txpGlobalSettings)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Test.Pos.Block.Logic.Emulation (runEmulation, sudoLiftIO)
import           Test.Pos.Block.Logic.Mode (BlockTestContext, BlockTestMode, TestParams (..), initBlockTestContext, runBlockTestMode)

-- | Criterion configuration
config :: Config
config = defaultConfig
    { reportFile = Just "verification.html"
    , resamples = 10
    , timeLimit = 5.0
    , verbosity = Verbose
    }

genesisInitializer :: GenesisInitializer
genesisInitializer = GenesisInitializer
    { giTestBalance = balance
    , giFakeAvvmBalance = FakeAvvmOptions
          { faoCount = 1
          , faoOneBalance = maxBound
          }
    , giAvvmBalanceFactor = unsafeCoinPortionFromDouble 0
    , giUseHeavyDlg = False
    , giSeed = 0
    }

balance :: TestnetBalanceOptions
balance = TestnetBalanceOptions
    { tboPoors = 1
    , tboRichmen = 1
    , tboTotalBalance = maxBound
    , tboRichmenShare = 1
    , tboUseHDAddresses = False
    }

runBTM
    :: TestParams
    -> BlockTestContext
    -> BlockTestMode a
    -> IO a
runBTM tp ctx btm = runEmulation (getTimestamp (_tpStartTime tp)) $ runReaderT btm ctx

-- | Benchmark which runs `verifyAndApplyBlocks` && `rollbackBlocks`.
verifyBlocksBenchmark
    :: HasConfigurations
    => ProtocolMagic
    -> TestParams
    -> BlockTestContext
    -> Benchmark
verifyBlocksBenchmark !pm !tp !ctx =
    bgroup "block verification"
        [ env (runBlockTestMode tp (genEnv (BlockCount 100)))
            $ \ ~(vctx, blocks) -> bench "verifyAndApplyBlocks" (verifyAndApplyBlocksB vctx blocks)
        -- `verifyBlocksPrefix` will succeed only on the first block, it
        -- requires that blocks are applied.
        , env (runBlockTestMode tp (genEnv (BlockCount 1)))
            $ \ ~(vctx, blocks) -> bench "verifyBlocksPrefix" (verifyBlocksPrefixB vctx blocks)
        ]
    where
    genEnv :: BlockCount -> BlockTestMode (VerifyBlocksContext, OldestFirst NE Block)
    genEnv bCount = do
        initNodeDBs pm slotSecurityParam
        g <- liftIO $ newStdGen
        let secretKeys = case genesisSecretKeys of
                Nothing -> error "verifyAndApplyBlocksBenchmark: no genesisSecretKeys"
                Just ks -> ks
        bs <- flip evalRandT g $ genBlocksNoApply pm
                (BlockGenParams
                    { _bgpSecrets = mkAllSecretsSimple secretKeys
                    , _bgpBlockCount = bCount
                    , _bgpTxGenParams = TxGenParams
                        { _tgpTxCountRange = (0, 2)
                        , _tgpMaxOutputs = 2
                        }
                    , _bgpInplaceDB = False
                    , _bgpSkipNoKey = True -- TODO: should be False?
                    , _bgpGenStakeholders = gdBootStakeholders genesisData
                    , _bgpTxpGlobalSettings = txpGlobalSettings pm
                    })
                maybeToList
        let curSlot :: Maybe SlotId
            curSlot = case catMaybes
                . map (either (const Nothing) Just . unEpochOrSlot . getEpochOrSlot)
                $ bs of
                [] -> Nothing
                ss -> Just $ maximum ss
        vctx <- getVerifyBlocksContext' curSlot
        return $ ( vctx, OldestFirst $ NE.fromList bs)

    verifyAndApplyBlocksB
        :: VerifyBlocksContext
        -> OldestFirst NE Block
        -> Benchmarkable
    verifyAndApplyBlocksB verifyBlocksCtx blocks =
        nfIO
            $ runBTM tp ctx
            $ do
                verifyAndApplyBlocks pm verifyBlocksCtx False blocks >>= \case
                    Left err -> return (Just err)
                    Right (_, blunds) -> do
                        whenJust (nonEmptyNewestFirst blunds) (rollbackBlocks pm)
                        return Nothing

    verifyBlocksPrefixB
        :: VerifyBlocksContext
        -> OldestFirst NE Block
        -> Benchmarkable
    verifyBlocksPrefixB verifyBlocksCtx blocks =
        nfIO
            $ runBTM tp ctx
            $ map fst <$> verifyBlocksPrefix pm verifyBlocksCtx blocks

-- | Benchmark which runs `verifyHeader`
verifyHeaderBenchmark
    :: HasConfigurations
    => ProtocolMagic
    -> TestParams
    -> Benchmark
verifyHeaderBenchmark !pm !tp = bgroup "verifyHeader"
    [ env (runBlockTestMode tp genEnv)
        $ \e -> bench "verifyHeader" (benchHeaderVerification e)
    ]
    where
    genEnv :: BlockTestMode (SlotLeaders, BlockHeader)
    genEnv = do
        initNodeDBs pm slotSecurityParam
        g <- liftIO $ newStdGen
        eos <- getEpochOrSlot <$> getTipHeader
        let epoch = eos ^. epochIndexL
        let secretKeys = case genesisSecretKeys of
                Nothing -> error "verifyHeaderBench: no genesisSecretKeys"
                Just ks -> ks
        let blockGenParams = BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple secretKeys
                , _bgpBlockCount = BlockCount 1
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True -- TODO: should be False?
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpTxpGlobalSettings = txpGlobalSettings pm
                }
        leaders <- lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeadersForEpoch
        mblock <- flip evalRandT g $ do
            blockGenCtx <- lift $ mkBlockGenContext blockGenParams
            tipHeader <- lift $ getTipHeader
            mapRandT (flip runReaderT blockGenCtx)
                $ genBlockNoApply pm eos tipHeader
        let !block = fromMaybe (error "verifyHeaderBench: failed to generate a header") mblock
        return (leaders, getBlockHeader block)

    benchHeaderVerification
        :: (SlotLeaders, BlockHeader)
        -> Benchmarkable
    benchHeaderVerification ~(leaders, header) =
        let !verifyHeaderParams = VerifyHeaderParams
                { vhpPrevHeader = Nothing
                , vhpCurrentSlot = Nothing
                , vhpLeaders = Just leaders
                , vhpMaxSize = Nothing
                , vhpVerifyNoUnknown = False
                }
        in nf isVerSuccess $ verifyHeader pm verifyHeaderParams header

runBenchmark :: IO ()
runBenchmark = do
    startTime <- realTime
    let cfo = defaultConfigurationOptions
            { cfoFilePath = "../lib/configuration.yaml"
            , cfoKey = "bench-validation"
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo $
        withConfigurationsM (LoggerName "verifyBenchmark") Nothing cfo $ \_ pm -> do
                let tp = TestParams
                        { _tpStartTime = Timestamp (convertUnit startTime)
                        , _tpBlockVersionData = genesisBlockVersionData
                        , _tpGenesisInitializer = genesisInitializer
                        }
                runEmulation startTime
                    $ initBlockTestContext tp $ \ctx ->
                        sudoLiftIO $ defaultMainWith config
                            [ verifyBlocksBenchmark pm tp ctx
                            , verifyHeaderBenchmark pm tp
                            ]