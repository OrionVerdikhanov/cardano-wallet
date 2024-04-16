{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Cardano.Write.Tx.ResolvedTx
    ( type ResolvedTx
    , pattern ResolvedTx
    , isValid
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( EraTx (bodyTxL)
    , EraTxBody (inputsTxBodyL)
    )
import Control.Lens
    ( over
    , view
    )
import Data.Set
    ( Set
    )
import Internal.Cardano.Write.Tx
    ( IsRecentEra
    , Tx
    , TxIn
    , UTxO (UTxO)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | A transaction whose inputs can all be resolved by the associated UTxO set.
--
-- The UTxO set may also contain additional UTxOs that are not referenced by
-- the transaction.
--
data ResolvedTx era = UnsafeResolvedTx (UTxO era) (Tx era)

{-# COMPLETE ResolvedTx #-}
pattern ResolvedTx :: IsRecentEra era => UTxO era -> Tx era -> ResolvedTx era
pattern
    ResolvedTx utxo tx <- UnsafeResolvedTx utxo tx where
    ResolvedTx utxo tx = construct utxo tx

construct
    :: forall era. IsRecentEra era
    => UTxO era
    -> Tx era
    -> ResolvedTx era
construct utxo@(UTxO utxoMap) tx0 = UnsafeResolvedTx utxo tx
  where
    tx :: Tx era
    tx = over (bodyTxL . inputsTxBodyL) (Set.filter (`Map.member` utxoMap)) tx0

isValid :: IsRecentEra era => ResolvedTx era -> Bool
isValid = (== Set.empty) . unresolvedInputs

unresolvedInputs :: IsRecentEra era => ResolvedTx era -> Set TxIn
unresolvedInputs (ResolvedTx (UTxO utxoMap) tx) =
    Set.filter
        (`Map.notMember` utxoMap)
        (view (bodyTxL . inputsTxBodyL) tx)
