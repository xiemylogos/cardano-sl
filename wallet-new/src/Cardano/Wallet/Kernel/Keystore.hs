{-- | An opaque handle to a keystore, used to read and write 'EncryptedSecretKey'
      from/to disk.

    NOTE: This module aims to provide a stable interface with a concrete
    implementation concealed by the user of this module. The internal operations
    are currently quite inefficient, as they have to work around the legacy
    'UserSecret' storage.
--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.Keystore (
      Keystore -- opaque
      -- * Constructing a keystore
    , newKeystore
    , newLegacyKeystore
    -- * Destroying a keystore (something you rarely should do)
    , destroyKeystore
    -- * Releasing a keystore and its associated resources
    , releaseKeystore
    -- * Inserting values
    , insert
    -- * Deleting values
    , delete
    -- * Queries on a keystore
    , lookup
    -- * Conversions
    , toList
    -- * Tests handy functions
    , newInMemoryTransientKeystore
    ) where

import           Universum hiding (toList)

import           Control.Concurrent (modifyMVar, modifyMVar_, withMVar)
import           Control.Lens (mapped)
import           Control.Monad.Trans.Identity (IdentityT (..), runIdentityT)
import qualified Data.List
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO (hClose, openTempFile)

import           Pos.Context (NodeContext (ncUserSecret))
import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Util.UserSecret (UserSecret, getUSPath, takeUserSecret,
                     usKeys, writeUserSecretRelease)
import           System.Wlog (CanLog (..), HasLoggerName (..), LoggerName (..),
                     logMessage)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Types (WalletId (..))

data StorageState =
      StorageReleased
      -- ^ The internal storage was released
    | StorageInitialised
      -- ^ The internal storage is initialised

-- Internal storage necessary to smooth out the legacy 'UserSecret' API.
data InternalStorage = InternalStorage {
      _storageSecret :: !UserSecret
    , _storageState  :: !StorageState
    }

data Keystore = Keystore (MVar InternalStorage)

-- | Internal monad used to smooth out the 'WithLogger' dependency imposed
-- by 'Pos.Util.UserSecret', to not commit to any way of logging things just yet.
newtype KeystoreM a = KeystoreM { fromKeystore :: IdentityT IO a }
                    deriving (Functor, Applicative, Monad, MonadIO)

instance HasLoggerName KeystoreM where
    askLoggerName = return (LoggerName "Keystore")
    modifyLoggerName _ action = action

instance CanLog KeystoreM where
    dispatchMessage _ln sev txt = logMessage sev txt

{-------------------------------------------------------------------------------
  Creating a keystore
-------------------------------------------------------------------------------}

-- | Creates a new keystore.
newKeystore :: MonadIO m => FilePath -> m Keystore
newKeystore fp = liftIO $ runIdentityT $ fromKeystore $ do
    us <- takeUserSecret fp
    Keystore <$> newMVar (InternalStorage us StorageInitialised)

-- | Creates a legacy 'Keystore' by reading the 'UserSecret' from a 'NodeContext'.
-- Hopefully this function will go in the near future.
newLegacyKeystore :: MonadIO m => NodeContext -> m Keystore
newLegacyKeystore ctx = do
     us <- atomically $ readTVar $ ncUserSecret ctx
     Keystore <$> newMVar (InternalStorage us StorageInitialised)

-- | Creates a 'Keystore' out of a randomly generated temporary file (i.e.
-- inside your $TMPDIR of choice). Suitable for testing.
newInMemoryTransientKeystore :: MonadIO m => m Keystore
newInMemoryTransientKeystore = liftIO $ runIdentityT $ fromKeystore $ do
    tempDir         <- liftIO getTemporaryDirectory
    (tempFile, hdl) <- liftIO $ openTempFile tempDir "keystore.key"
    liftIO $ hClose hdl
    us <- takeUserSecret tempFile
    Keystore <$> newMVar (InternalStorage us StorageInitialised)


-- | Release the resources associated with this 'Keystore'.
-- This function is idempotent and can be called multiple times.
releaseKeystore :: MonadIO m => Keystore -> m ()
releaseKeystore (Keystore ks) = liftIO $ modifyMVar ks $ \(InternalStorage us st) ->
    case st of
         StorageReleased      -> return ((InternalStorage us StorageReleased), ())
         StorageInitialised   -> do
             writeUserSecretRelease us
             return ((InternalStorage us StorageReleased), ())


{-------------------------------------------------------------------------------
  Destroying a keystore
-------------------------------------------------------------------------------}

-- | Destroys a 'Keystore'.
-- Completely obliterate the keystore from disk, with all its secrets.
-- This operation cannot be reverted.
-- This is a very destructive option that most of the time you
-- probably don't want, as it will effectively leave you with an empty
-- 'MVar' and your next call to an operation on the keystore would block
-- indefinitely.
-- This has still its use in some teardowns or tests.
-- Note that this operation will always succeed. Use with care.
destroyKeystore :: MonadIO m => Keystore -> m ()
destroyKeystore (Keystore ks) = liftIO $ do
    (InternalStorage us _st) <- takeMVar ks
    writeUserSecretRelease us
    removeFile (getUSPath us)

{-------------------------------------------------------------------------------
  Inserting things inside a keystore
-------------------------------------------------------------------------------}

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: MonadIO m
       => WalletId
       -> EncryptedSecretKey
       -> Keystore
       -> m ()
insert _walletId esk (Keystore ks) =
    liftIO $ modifyMVar_ ks $ \(InternalStorage us st) -> do
        return $ if view usKeys us `contains` esk
                     then InternalStorage us st
                     else InternalStorage (us & over usKeys (esk :)) st
    where
      -- Comparator taken from the old code which needs to hash
      -- all the 'EncryptedSecretKey' in order to compare them.
      contains :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
      contains ls k = hash k `elem` map hash ls

{-------------------------------------------------------------------------------
  Looking up things inside a keystore
-------------------------------------------------------------------------------}

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookup :: MonadIO m
       => WalletId
       -> Keystore
       -> m (Maybe EncryptedSecretKey)
lookup (WalletIdHdRnd walletId) (Keystore ks) =
    liftIO $ withMVar ks $ \(InternalStorage us st) ->
        return $ case st of
            StorageInitialised ->
                Data.List.find (\k -> eskToHdRootId k == walletId) (us ^. usKeys)
            StorageReleased      -> Nothing

{-------------------------------------------------------------------------------
  Deleting things from the keystore
-------------------------------------------------------------------------------}
delete :: MonadIO m
       => WalletId
       -> Keystore
       -> m ()
delete walletId (Keystore ks) = do
    mbEsk <- lookup walletId (Keystore ks)
    liftIO $ modifyMVar_ ks $ \(InternalStorage us st) ->
        case st of
            StorageInitialised -> do
                let erase = Data.List.deleteBy (\a b -> hash a == hash b)
                let us' = maybe us (\esk -> us & over usKeys (erase esk)) mbEsk
                return (InternalStorage us' st)
            StorageReleased -> return (InternalStorage us st)

-- | Returns all the 'EncryptedSecretKey' known to this 'Keystore'.
toList :: MonadIO m => Keystore -> m [(WalletId, EncryptedSecretKey)]
toList (Keystore ks) =
    liftIO $ withMVar ks $ \(InternalStorage us _st) ->
        return (over mapped (\k -> (WalletIdHdRnd (eskToHdRootId k), k)) (us ^. usKeys))
