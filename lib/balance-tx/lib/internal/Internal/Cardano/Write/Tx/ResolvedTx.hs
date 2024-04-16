module ResolvedTx where

data ResolvedTx era = UnsafeResolvedTx
    { __tx :: Tx era
    , __utxo :: UTxO era
    }
