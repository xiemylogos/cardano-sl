-- | Types describing runtime errors related to Txp.

module Pos.Txp.Error
       ( TxpError (..)
       ) where

import           Control.Exception.Safe (Exception (..))
import           Formatting (bprint, stext, (%))
import           Formatting.Buildable (Buildable (build))
import           Universum

import           Pos.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)

import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)

data TxpError
    = TxpInternalError !Text
    -- ^ Something bad happened inside Txp
    deriving (Show)

instance Exception TxpError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . toStrict . toLazyText . build

instance Buildable TxpError where
    build (TxpInternalError msg) =
        bprint ("internal error in Transaction processing: "%stext) msg
