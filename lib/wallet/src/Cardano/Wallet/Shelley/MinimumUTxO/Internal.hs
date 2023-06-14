{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Computing minimum UTxO values: internal interface.
--
module Cardano.Wallet.Shelley.MinimumUTxO.Internal
    ( computeMinimumCoinForUTxO_CardanoApi
    , computeMinimumCoinForUTxO_CardanoLedger
    ) where

import Prelude

import Cardano.Api
    ( ProtocolParametersConversionError )
import Cardano.Ledger.Api
    ( getMinCoinTxOut )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxOForShelleyBasedEra (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut )
import Cardano.Wallet.Shelley.Compatibility
    ( toCardanoTxOut, unsafeLovelaceToWalletCoin )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toAllegraTxOut, toAlonzoTxOut, toBabbageTxOut, toConwayTxOut, toMaryTxOut,
    toShelleyTxOut, toWalletCoin )
import Control.Lens
    ( (<&>) )
import Data.Function
    ( (&) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Api.Shelley as Cardano

-- | Computes a minimum UTxO value with the Cardano API.
--
-- Caution:
--
-- This function does /not/ attempt to reach a fixed point before returning its
-- result.
computeMinimumCoinForUTxO_CardanoApi
    :: HasCallStack
    => MinimumUTxOForShelleyBasedEra
    -> TxOut
    -> Coin
computeMinimumCoinForUTxO_CardanoApi
    (MinimumUTxOForShelleyBasedEra era pp)
    txOut =
        unsafeCoinFromResult
            $ Cardano.bundleProtocolParams
                (Cardano.shelleyBasedToCardanoEra era)
                (Cardano.fromLedgerPParams era pp)
            <&> Cardano.calculateMinimumUTxO
                era
                (toCardanoTxOut era txOut)
      where
        unsafeCoinFromResult
            :: Either ProtocolParametersConversionError Cardano.Lovelace
            -> Coin
        unsafeCoinFromResult = \case
            Right value ->
                -- We assume that the returned value is a non-negative ada quantity
                -- with no other assets. If this assumption is violated, we have no
                -- way to continue, and must raise an error:
                value
                    & unsafeLovelaceToWalletCoin
            Left e ->
                -- The 'Cardano.calculateMinimumUTxO' function should only return
                -- an error if a required protocol parameter is missing.
                --
                -- However, given that values of 'MinimumUTxOForShelleyBasedEra'
                -- can only be constructed by supplying an era-specific protocol
                -- parameters record, it should be impossible to trigger this
                -- condition.
                --
                -- Any violation of this assumption indicates a programming error.
                -- If this condition is triggered, we have no way to continue, and
                -- must raise an error:
                --
                error
                    $ unwords
                        [ "computeMinimumCoinForUTxO_CardanoApi:"
                        , "unexpected error:"
                        , show e
                        ]

-- | Computes a minimum UTxO value with Cardano Ledger.
--
-- Caution:
--
-- This function does /not/ attempt to reach a fixed point before returning its
-- result.
computeMinimumCoinForUTxO_CardanoLedger
    :: MinimumUTxOForShelleyBasedEra
    -> TxOut
    -> Coin
computeMinimumCoinForUTxO_CardanoLedger
    (MinimumUTxOForShelleyBasedEra era pp)
    txOut =
        toWalletCoin $ case era of
            Cardano.ShelleyBasedEraShelley ->
                getMinCoinTxOut pp
                    $ toShelleyTxOut txOut
            Cardano.ShelleyBasedEraAllegra ->
                getMinCoinTxOut pp
                    $ toAllegraTxOut txOut
            Cardano.ShelleyBasedEraMary ->
                getMinCoinTxOut pp
                    $ toMaryTxOut txOut
            Cardano.ShelleyBasedEraAlonzo ->
                getMinCoinTxOut pp
                    $ toAlonzoTxOut txOut
            Cardano.ShelleyBasedEraBabbage ->
                getMinCoinTxOut pp
                    $ toBabbageTxOut txOut
            Cardano.ShelleyBasedEraConway ->
                getMinCoinTxOut pp
                    $ toConwayTxOut txOut
