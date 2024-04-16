{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Cardano.Write.Tx.ResolvedTx
    ( type ResolvedTx
    , pattern ResolvedTx
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( EraTx (bodyTxL)
    , EraTxBody (inputsTxBodyL)
    )
import Control.Lens
    ( over
    )
import Internal.Cardano.Write.Tx
    ( IsRecentEra
    , Tx
    , UTxO (UTxO)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data ResolvedTx era = UnsafeResolvedTx (UTxO era) (Tx era)

pattern ResolvedTx :: IsRecentEra era => UTxO era -> Tx era -> ResolvedTx era
pattern
    ResolvedTx utxo tx <- UnsafeResolvedTx utxo tx where
    ResolvedTx utxo tx = makeResolvedTx utxo tx

makeResolvedTx
    :: forall era. IsRecentEra era
    => UTxO era
    -> Tx era
    -> ResolvedTx era
makeResolvedTx utxo@(UTxO utxoMap) tx0 = UnsafeResolvedTx utxo tx
  where
    tx :: Tx era
    tx = over (bodyTxL . inputsTxBodyL) (Set.filter (`Map.member` utxoMap)) tx0
