module Pos.Core.Txp.TxMsg
       ( TxMsgContents (..)
       ) where

import           Universum

import qualified Formatting.Buildable
import           Formatting (bprint, (%), Buildable)

import           Pos.Core.Txp.TxAux (TxAux (..), txaF)

-- | Data message. Can be used to send one transaction per message.
-- Transaction is sent with auxilary data.
newtype TxMsgContents = TxMsgContents
    { getTxMsgContents :: TxAux
    } deriving (Generic, Show, Eq)

instance Buildable TxMsgContents where
    build (TxMsgContents txAux) =
        bprint ("TxMsgContents { txAux ="%txaF%", .. }") txAux
