module Pos.Core.Update.SoftwareVersion
       ( SoftwareVersion (..)
       , HasSoftwareVersion (..)
       , NumSoftwareVersion
       , checkSoftwareVersion
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, int, stext, (%))
import           Formatting.Buildable (Buildable (build))
import qualified Prelude

import           Pos.Util.Some (Some, liftLensSome)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Update.ApplicationName

import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
----------------------------------------------------------------------------
-- Compat shims
----------------------------------------------------------------------------
-- pretty used to be in Universum
pretty :: Buildable a => a -> Text
pretty = toStrict . toLazyText . build

-- | Numeric software version associated with ApplicationName.
type NumSoftwareVersion = Word32

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)

instance Buildable SoftwareVersion where
    build SoftwareVersion {..} =
        bprint (stext % ":" % int) (getApplicationName svAppName) svNumber

instance Show SoftwareVersion where
    show = toString . pretty

instance Hashable SoftwareVersion

instance NFData SoftwareVersion

deriveSafeCopySimple 0 'base ''SoftwareVersion

-- | A software version is valid iff its application name is valid.
checkSoftwareVersion :: MonadError Text m => SoftwareVersion -> m ()
checkSoftwareVersion sv = checkApplicationName (svAppName sv)

class HasSoftwareVersion a where
    softwareVersionL :: Lens' a SoftwareVersion

instance HasSoftwareVersion (Some HasSoftwareVersion) where
    softwareVersionL = liftLensSome softwareVersionL

deriveSimpleBi ''SoftwareVersion [
    Cons 'SoftwareVersion [
        Field [| svAppName :: ApplicationName    |],
        Field [| svNumber  :: NumSoftwareVersion |]
    ]]
