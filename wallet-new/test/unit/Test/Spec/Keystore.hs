{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.Keystore (
    spec
  ) where

import           Universum

import           System.Directory (doesFileExist, removeFile)
import           System.IO.Error (IOError)

import           Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, arbitrary)
import           Test.QuickCheck.Monadic (forAllM, monadicIO, pick, run)

import           Pos.Crypto (EncryptedSecretKey, hash, safeKeyGen)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))

import           Util.Buildable (ShowThroughBuild (..))

-- | Creates, operate on a keystore and finally destroys it.
withKeystore :: (MonadIO m, MonadMask m) => (Keystore -> m a) -> m a
withKeystore action =
    bracket (Keystore.newInMemoryTransientKeystore)
            (\ks -> Keystore.releaseKeystore ks >>
                    Keystore.destroyKeystore ks
            )
            action

genKeypair :: Gen ( ShowThroughBuild WalletId
                  , ShowThroughBuild EncryptedSecretKey
                  )
genKeypair = do
    (_, esk) <- arbitrary >>= safeKeyGen
    return $ bimap STB STB (WalletIdHdRnd . eskToHdRootId  $ esk, esk)

nukeKeystore :: FilePath -> IO ()
nukeKeystore fp =
    removeFile fp `catch` (\(_ :: IOError) -> return ())

spec :: Spec
spec =
    describe "Keystore to store UserSecret(s)" $ do
        it "creating a brand new one works" $ do
            nukeKeystore "test_keystore.key"
            keystore <- Keystore.newKeystore "test_keystore.key"
            doesFileExist "test_keystore.key" `shouldReturn` True
            Keystore.releaseKeystore keystore

        it "destroying a keystore (completely) works" $ do
            keystore <- Keystore.newKeystore "test_keystore.key"
            Keystore.destroyKeystore keystore
            doesFileExist "test_keystore.key" `shouldReturn` False

        prop "lookup of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "Inserts are persisted after releasing the keystore" $ monadicIO $ do
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore "test_keystore.key"
                keystore1 <- Keystore.newKeystore "test_keystore.key"
                Keystore.insert wid esk keystore1
                Keystore.releaseKeystore keystore1
                keystore2 <- Keystore.newKeystore "test_keystore.key"
                mbKey <- Keystore.lookup wid keystore2
                (fmap hash mbKey) `shouldBe` (Just (hash esk))
                -- Let's not forget to tear down the keystore.
                Keystore.releaseKeystore keystore2

        prop "deletion of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    Keystore.delete wid ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` Nothing

        prop "Deletion of keys is idempotent" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    Keystore.delete wid ks
                    Keystore.delete wid ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` Nothing

        prop "Deletion of keys are persisted after releasing the keystore" $ monadicIO $ do
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore "test_keystore.key"
                keystore1 <- Keystore.newKeystore "test_keystore.key"
                Keystore.insert wid esk keystore1
                Keystore.delete wid keystore1
                Keystore.releaseKeystore keystore1
                keystore2 <- Keystore.newKeystore "test_keystore.key"
                mbKey <- Keystore.lookup wid keystore2
                (fmap hash mbKey) `shouldBe` Nothing
                -- Let's not forget to tear down the keystore.
                Keystore.releaseKeystore keystore2
