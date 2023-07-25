module Cardano.Wallet.Transaction.Built
  ( BuiltTx (..)
  )
where

import Cardano.Wallet.Primitive.Types.Tx
  ( SealedTx
  , Tx
  , TxMeta
  )
import Prelude

data BuiltTx = BuiltTx
  { builtTx :: Tx
  , builtTxMeta :: TxMeta
  , builtSealedTx :: SealedTx
  }
  deriving (Show, Eq)
