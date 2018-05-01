module Lang.Name
       ( Letter(getLetter)
       , unsafeMkLetter
       , Name(..)
       , unsafeMkName
       ) where

import           Prelude (Show (..))
import           Universum

import           Data.Char (isAlpha)
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Split (splitWhen)

import           Test.QuickCheck.Arbitrary.Generic (Arbitrary (..))
import           Test.QuickCheck.Gen (Gen, listOf, suchThat)

import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Formatting.Buildable (Buildable (build))
----------------------------------------------------------------------------
-- Compat shims
----------------------------------------------------------------------------
-- pretty used to be in Universum
pretty :: Buildable a => a -> Text
pretty = toStrict . toLazyText . build

-- | Invariant: @isAlpha . getLetter = const True@
newtype Letter = Letter { getLetter :: Char }
    deriving (Eq, Ord, Show)

unsafeMkLetter :: Char -> Letter
unsafeMkLetter = Letter

instance Arbitrary Letter where
    arbitrary = Letter <$> arbitrary `suchThat` isAlpha

newtype Name = Name (NonEmpty (NonEmpty Letter))
    deriving (Eq, Ord, Generic)

unsafeMkName :: [String] -> Name
unsafeMkName = coerce . fmap NonEmpty.fromList . NonEmpty.fromList

instance Arbitrary Name where
    arbitrary = Name <$> neList (neList arbitrary)
      where
        neList :: Gen a -> Gen (NonEmpty a)
        neList gen = (:|) <$> gen <*> listOf gen

instance Buildable Name where
    build
        = foldMap (fromString . toList)
        . NonEmpty.intersperse ('-' :| [])
        . fromLetterNENE
      where
        fromLetterNENE :: Name -> NonEmpty (NonEmpty Char)
        fromLetterNENE = coerce

instance Show Name where
    showsPrec n = showsPrec n . pretty

-- | Unsafe, requires manual validation.
instance IsString Name where
    fromString = unsafeMkName . splitWhen (=='-')
