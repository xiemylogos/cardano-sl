{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Formatting (bprint, int, sformat, shown, (%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (build))
import           Mockable (mapConcurrently)
import           Serokell.Util (listJson)
import           System.Wlog (WithLogger, askLoggerName, logInfo)

import           Pos.Context (getOurPublicKey)
import           Pos.Core (GenesisData (gdBootStakeholders, gdHeavyDelegation),
                     GenesisDelegation (..), GenesisWStakeholders (..),
                     addressHash, gdFtsSeed, genesisData)
import           Pos.Crypto (ProtocolMagic, pskDelegatePk)
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.GState as GS
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Reporting (reportError)
import           Pos.Infra.Slotting (waitSystemStart)
import           Pos.Infra.Util.LogSafe (logInfoS)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Txp (bootDustThreshold)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration,
                     curSoftwareVersion, lastKnownBlockVersion, ourSystemTag)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Pos.Worker (allWorkers)
import           Pos.WorkMode.Class (WorkMode)

import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
----------------------------------------------------------------------------
-- Compat shims
----------------------------------------------------------------------------
-- pretty used to be in Universum
pretty :: Buildable a => a -> Text
pretty = toStrict . toLazyText . build

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ext ctx m.
       ( HasCompileInfo
       , WorkMode ctx m
       )
    => NodeResources ext
    -> [Diffusion m -> m ()]
    -> [Diffusion m -> m ()]
    -> Diffusion m -> m ()
runNode' NodeResources {..} workers' plugins' = \diffusion -> do
    logInfo $ "Built with: " <> pretty compileInfo
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%F.build%", pk hash: "%F.build)
        pk pkHash

    let genesisStakeholders = gdBootStakeholders genesisData
    logInfo $ sformat
        ("Genesis stakeholders ("%int%" addresses, dust threshold "%F.build%"): "%F.build)
        (length $ getGenesisWStakeholders genesisStakeholders)
        bootDustThreshold
        genesisStakeholders

    let genesisDelegation = gdHeavyDelegation genesisData
    let formatDlgPair (issuerId, delegateId) =
            bprint (F.build%" -> "%F.build) issuerId delegateId
    logInfo $ sformat ("GenesisDelegation (stakeholder ids): "%listJson)
            $ map (formatDlgPair . second (addressHash . pskDelegatePk))
            $ HM.toList
            $ unGenesisDelegation genesisDelegation

    firstGenesisHash <- GS.getFirstGenesisBlockHash
    logInfo $ sformat
        ("First genesis block hash: "%F.build%", genesis seed is "%F.build)
        firstGenesisHash
        (gdFtsSeed genesisData)

    tipHeader <- DB.getTipHeader
    logInfo $ sformat ("Current tip header: "%F.build) tipHeader

    waitSystemStart
    let runWithReportHandler action =
            action diffusion `catch` reportHandler

    void (mapConcurrently runWithReportHandler (workers' ++ plugins'))

    exitFailure

  where
    -- FIXME shouldn't this kill the whole program?
    -- FIXME: looks like something bad.
    -- REPORT:ERROR Node's worker/plugin failed with exception (which wasn't caught)
    reportHandler (SomeException e) = do
        loggerName <- askLoggerName
        reportError $
            sformat ("Worker/plugin with logger name "%shown%
                    " failed with exception: "%shown)
            loggerName e

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: ( HasCompileInfo
       , HasTxpConfiguration
       , WorkMode ctx m
       )
    => ProtocolMagic
    -> NodeResources ext
    -> [Diffusion m -> m ()]
    -> Diffusion m -> m ()
runNode pm nr plugins = runNode' nr workers' plugins
  where
    workers' = allWorkers pm nr

-- | This function prints a very useful message when node is started.
nodeStartMsg :: (HasUpdateConfiguration, WithLogger m) => m ()
nodeStartMsg = logInfo msg
  where
    msg = sformat ("Application: " %F.build% ", last known block version "
                    %F.build% ", systemTag: " %F.build)
                   curSoftwareVersion
                   lastKnownBlockVersion
                   ourSystemTag
