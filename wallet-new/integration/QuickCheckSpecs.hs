module QuickCheckSpecs where

import qualified Prelude
import           Universum

import Control.Concurrent (threadDelay)
import           Data.Default (def)
import           Data.List.NonEmpty (fromList)
import           Data.Maybe (fromJust)

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Formatting (format, shown, string, (%))
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types
import           System.Wlog (HasLoggerName (..), LoggerName (..))

import           Pos.Block.Types (Blund)
import           Pos.Client.CLI (CommonArgs (..), CommonNodeArgs (..), NodeArgs (..), getNodeParams,
                                 gtSscParams)
import           Pos.Core (GenesisBlock, MainBlock, Timestamp (..), protocolMagic, ProtocolMagic, headerHash)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Rocks.Functions (openNodeDBs)
import           Pos.DB.Rocks.Types (NodeDBs)
import qualified Pos.Diffusion.Types as D
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeResources (..),
                               allocateNodeResources, defaultConfigurationOptions, npBehaviorConfig,
                               npUserSecret, withConfigurations)
import           Pos.Network.CLI (NetworkConfigOpts (..))
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)

import           Cardano.Wallet.API.V1.LegacyHandlers.Accounts (newAccount)
import           Cardano.Wallet.API.V1.LegacyHandlers.Addresses (newAddress)
import           Cardano.Wallet.API.V1.LegacyHandlers.Wallets (newWallet)

import           Pos.DB.GState.Common (getTip, initGStateCommon, setInitialized)
import           Pos.StateLock (StateLock (..))
import           Pos.Wallet.Web.Mode (MonadWalletWebMode, WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.State (WalletDB, openState)

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import           Ntp.Client (withoutNtpClient)

import           Pos.WorkMode (RealModeContext (..))

import           Pos.DB.Block (prepareBlockDB, putBlunds)

import           Pos.Util.Util (lensOf)
import           System.Directory (createDirectoryIfMissing, doesPathExist, getCurrentDirectory,
                                   removeDirectoryRecursive)

import           TestUtil (BlockNumber, SlotsPerEpoch, createEmptyUndo,
                           produceBlocksByBlockNumberAndSlots, produceSecretKeys,
                           produceSlotLeaders)

import           Mockable (Production, runProduction)
import           Pos.Util.JsonLog (jsonLogConfigFromHandle)
import           Pos.Util.UserSecret (usVss)

import           Servant
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal

import           System.Directory

import           Test.Hspec
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import qualified Cardano.Wallet.API.V1.Migration as Migration
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types (Wallet (..))

--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest (apiType a :> sub) =>
         HasGenRequest (WithDefaultApiArg apiType a :> sub) where
    genRequest _ = genRequest (Proxy @(apiType a :> sub))

instance HasGenRequest (argA a :> argB a :> sub) =>
         HasGenRequest (AlternativeApiArg argA argB a :> sub) where
    genRequest _ = genRequest (Proxy @(argA a :> argB a :> sub))

-- NOTE(adinapoli): This can be improved to produce proper filtering & sorting
-- queries.
instance HasGenRequest sub => HasGenRequest (SortBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (FilterBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest _ = genRequest (Proxy :: Proxy sub)

instance HasGenRequest (sub :: *) => HasGenRequest (WalletRequestParams :> sub) where
    genRequest _ = genRequest (Proxy @(WithWalletRequestParams sub))

--
-- RESTful-abiding predicates
--

-- | Checks that every DELETE request should return a 204 NoContent.
deleteReqShouldReturn204 :: RequestPredicate
deleteReqShouldReturn204 = RequestPredicate $ \req mgr ->
     if (method req == methodDelete)
       then do
         resp <- httpLbs req mgr
         let status = responseStatus resp
         when (statusIsSuccessful status && status /= status204) $
           throwM $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
         return [resp]
       else return []

-- | Checks that every PUT request is idempotent. Calling an endpoint with a PUT
-- twice should return the same result.
putIdempotency :: RequestPredicate
putIdempotency = RequestPredicate $ \req mgr ->
     if (method req == methodPut)
       then do
         resp1 <- httpLbs req mgr
         resp2 <- httpLbs req mgr
         let body1 = responseBody resp1
         let body2 = responseBody resp2
         when (body1 /= body2) $
           throwM $ PredicateFailure "putIdempotency" (Just req) resp1
         return [resp1, resp2]
       else return []

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: RequestPredicate
noEmptyBody = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  let body   = responseBody resp
  let status = responseStatus resp
  when (status /= status204 && body == mempty) $
    throwM $ PredicateFailure "noEmptyBody" (Just req) resp
  return [resp]

-- | All the predicates we want to enforce in our API.
predicates :: Predicates
predicates = not500
         <%> deleteReqShouldReturn204
         <%> putIdempotency
         <%> noEmptyBody
         <%> mempty

-- | "Lowers" V0 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v0Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => D.Diffusion Migration.MonadV1 -> IO (Server V0.API)
v0Server diffusion = do
  -- TODO(adinapoli): If the monadic stack ends up diverging between V0 and V1,
  -- it's obviously incorrect using 'testV1Context' here.
  ctx <- testV1Context
  withoutNtpClient $ \ntpStatus ->
    return (V0.handlers (Migration.v1MonadNat ctx) diffusion ntpStatus)

-- | "Lowers" V1 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v1Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => D.Diffusion Migration.MonadV1 -> IO (Server V1.API)
v1Server diffusion = do
  ctx <- testV1Context
  withoutNtpClient $ \ntpStatus ->
    return (V1.handlers (Migration.v1MonadNat ctx) diffusion ntpStatus)

-- | Get the current time from the system to provide a @systemStart@.
getCurrentTime :: MonadIO m => m Microsecond
getCurrentTime = liftIO $ fromMicroseconds . round . ( * 1000000) <$> getPOSIXTime

-- | Returns a test 'V1Context' which can be used for the API specs.
-- Such context will use an in-memory database.
testV1Context :: Migration.HasConfiguration => IO Migration.V1Context
testV1Context = do

    let configurationPath = "../lib/configuration.yaml"

    let testPath          = "../run/integration-test/"
    let nodeDBPath        = testPath <> "node-integration-db"
    let walletDBPath      = testPath <> "node-wallet-db"
    let secretKeyPath     = testPath <> "secret-integration-buahaha?.key"

    -- Let's first clear the test directory.
    whenM (doesPathExist testPath) $ removeDirectoryRecursive testPath

    -- Create it.
    createDirectoryIfMissing True testPath

    currentTime       <- getCurrentTime
    currentDirectory  <- getCurrentDirectory

    -- We want to have this since it can be useful if it fails.
    putStrLn $ format
        ("Integration test run on '" % shown % "', current dir - '" % string % "'.")
        currentTime
        currentDirectory

    let cfg = defaultConfigurationOptions
            { cfoSystemStart  = Just . Timestamp $ currentTime
            , cfoFilePath     = configurationPath
            , cfoKey          = "dev"
            }

    -- Open wallet state. Delete if exists.
    ws <- liftIO $ openState True walletDBPath

    liftIO $ withConfigurations cfg $ \_ntpConfig ->
        withCompileInfo $(retrieveCompileTimeInfo) $ do
            dbs  <- openNodeDBs False nodeDBPath

            -- We probably need to close this, but it should close
            -- after the test is done.
            walletRunner cfg dbs secretKeyPath ws
            -- closeState ws


-- | Here we have data that we need since we fetch it from the clinet.
insertRequiredData
    :: forall ctx. MonadWalletWebMode ctx WalletWebMode
    => ProtocolMagic
    -> WalletWebMode ()
insertRequiredData pm = do

    -- Generate arbitrary blockchain.
    (genBlock, mainBlocks) <- liftIO $ generateValidBlocks pm 10 5

    -- Insert the data we need in the node database.
    prepareBlockDB genBlock

    let blunds :: [Blund]
        blunds = map (\mb -> (Right mb, createEmptyUndo)) mainBlocks

    putBlunds $ fromList blunds

    initGStateCommon $ headerHash genBlock
    setInitialized

    -- Lock the access, we now insert what we need in the wallet database.
    gainExclusiveLock

    -- The idea here was that we were going to insert the required
    -- arbitrary instances in the wallet database and when the
    -- servant-quickcheck asks for them, we will have them.
    genWallet   <- liftIO $ generate arbitrary
    genAccount  <- liftIO $ generate arbitrary
    genAddress  <- liftIO $ generate arbitrary

    wallet      <- newWallet genWallet
    _account    <- newAccount (walId . wrData $ wallet) genAccount
    _address    <- newAddress genAddress

    pure ()


-- | Simple blocks generation. Currently using @Explorer@ block generation.
-- Ideally, we should use wallet-new block generation with something like:
-- `generate $ int . fpcChain =<< runTranslateT =<< fromPreChain =<< genValidBlockchain`
generateValidBlocks
    :: ProtocolMagic
    -> BlockNumber
    -> SlotsPerEpoch
    -> IO (GenesisBlock, [MainBlock])
generateValidBlocks pm blocksNumber slotsPerEpoch = do

    slotLeaders   <- produceSlotLeaders blocksNumber
    secretKeys    <- produceSecretKeys blocksNumber

    blocks        <- withDefConfigurations $ \_ntpConfiguration ->
        produceBlocksByBlockNumberAndSlots
            pm
            blocksNumber
            slotsPerEpoch
            slotLeaders
            secretKeys

    let genesisBlock :: GenesisBlock
        genesisBlock = Prelude.head $ lefts blocks

    let mainBlocks :: [MainBlock]
        mainBlocks = rights $ Prelude.tail blocks

    pure (genesisBlock, mainBlocks)


gainExclusiveLock
    :: forall ctx. MonadWalletWebMode ctx WalletWebMode
    => WalletWebMode ()
gainExclusiveLock = do
    tip <- getTip
    (StateLock mvar _) <- view (lensOf @StateLock)
    () <$ tryPutMVar mvar tip


-- | Required instance for IO.
instance HasLoggerName IO where
    askLoggerName = pure $ LoggerName "APISpec"
    modifyLoggerName _ x = x


newRealModeContext
    :: HasConfigurations
    => NodeDBs
    -> ConfigurationOptions
    -> FilePath
    -> Production (RealModeContext ())
newRealModeContext dbs confOpts secretKeyPath = do

    let nodeArgs = NodeArgs {
      behaviorConfigPath = Nothing
    }

    let networkOps = NetworkConfigOpts {
          ncoTopology = Nothing
        , ncoKademlia = Nothing
        , ncoSelf     = Nothing
        , ncoPort     = 3030
        , ncoPolicies = Nothing
        , ncoBindAddress = Nothing
        , ncoExternalAddress = Nothing
        }

    let cArgs@CommonNodeArgs {..} = CommonNodeArgs {
           dbPath                 = Just "node-db"
         , rebuildDB              = True
         , devGenesisSecretI      = Nothing
         , keyfilePath            = secretKeyPath
         , networkConfigOpts      = networkOps
         , jlPath                 = Nothing
         , commonArgs             = CommonArgs {
               logConfig            = Nothing
             , logPrefix            = Nothing
             , reportServers        = mempty
             , updateServers        = mempty
             , configurationOptions = confOpts
             }
         , updateLatestPath       = "update"
         , updateWithPackage      = False
         , route53Params          = Nothing
         , enableMetrics          = False
         , ekgParams              = Nothing
         , statsdParams           = Nothing
         , cnaDumpGenesisDataPath = Nothing
         , cnaDumpConfiguration   = False
         }

    loggerName <- askLoggerName
    nodeParams <- getNodeParams loggerName cArgs nodeArgs

    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)

    -- Maybe switch to bracketNodeResources?
    nodeResources <-  allocateNodeResources
                          nodeParams
                          gtParams
                          txpGlobalSettings
                          initNodeDBs

    RealModeContext <$> pure dbs
                    <*> pure (nrSscState nodeResources)
                    <*> pure (nrTxpState nodeResources)
                    <*> pure (nrDlgState nodeResources)
                    <*> jsonLogConfigFromHandle stdout
                    <*> pure (LoggerName "APISpec")
                    <*> pure (nrContext nodeResources)


-- | The runner we need to return out wallet context.
walletRunner
    :: (HasConfigurations, HasCompileInfo)
    => ConfigurationOptions
    -> NodeDBs
    -> FilePath
    -> WalletDB
    -> IO WalletWebModeContext
walletRunner confOpts dbs secretKeyPath ws = runProduction $ do
    wwmc  <- WalletWebModeContext <$> pure ws
                                  <*> newTVarIO def
                                  <*> liftIO STM.newTQueueIO
                                  <*> newRealModeContext dbs confOpts secretKeyPath

    liftIO (threadDelay (20 * 10e6))
    -- Insert the wallet and node data we need to run this test.
    runReaderT (insertRequiredData protocolMagic) wwmc
    pure wwmc

serverLayout :: ByteString
serverLayout = Text.encodeUtf8 (layout (Proxy @V1.API))

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Spec
spec = withCompileInfo def $ withDefConfigurations $ \_ -> do
    describe "Servant API Properties" $ do
        it "V0 API follows best practices & is RESTful abiding" $ do
            serverSatisfies (Proxy @V0.API) burl stdArgs predicates
        it "V1 API follows best practices & is RESTful abiding" $ do
            serverSatisfies (Proxy @V1.API) burl stdArgs predicates