{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Conway
    ( fromConwayTx
    , fromConwayTx'
    )
    where

import Prelude

import Cardano.Api
    ( ConwayEra
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    , addrTxWitsL
    , auxDataTxL
    , bodyTxL
    , bootAddrTxWitsL
    , collateralInputsTxBodyL
    , collateralReturnTxBodyL
    , conwayCertsTxBodyL
    , feeTxBodyL
    , inputsTxBodyL
    , isValidTxL
    , mintTxBodyL
    , outputsTxBodyL
    , referenceInputsTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Ledger.Babbage
    ( BabbageTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletScript
    , toWalletTokenPolicyId
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( fromConwayCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( fromConwayMetadata
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( conwayMint
    , fromLedgerScriptHash
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromConwayTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( fromLedgerWithdrawals
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( PlutusScriptInfo (PlutusScriptInfo)
    , PlutusVersion (PlutusVersionV1, PlutusVersionV2, PlutusVersionV3)
    , ReferenceInput (ReferenceInput)
    , ScriptReference (..)
    , TokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (txId)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    , WitnessCountCtx
    , toKeyRole
    )
import Cardano.Wallet.Read.Eras
    ( conway
    , inject
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash
    )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( shelleyWithdrawals
    )
import Control.Lens
    ( folded
    , (<&>)
    , (^.)
    , (^..)
    )
import Data.Foldable
    ( toList
    )
import Data.Map
    ( Map
    )
import Data.Maybe.Strict
    ( strictMaybeToMaybe
    )
import Data.Word
    ( Word32
    )
import Ouroboros.Consensus.Cardano.Block
    ( StandardConway
    )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Language as Language
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromConwayTx
    :: Alonzo.AlonzoTx (Cardano.ShelleyLedgerEra ConwayEra)
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromConwayTx tx witCtx =
    ( tx'
    , fmap fromConwayCerts . toList $ tx ^. bodyTxL . conwayCertsTxBodyL
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL.vldtTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL.addrTxWitsL)
        (Map.elems $ Map.union anyScriptsFromWits anyScriptsFromTxOuts)
        (fromIntegral $ Set.size $ tx ^. witsTxL.bootAddrTxWitsL)
    )
  where
    tx' = fromConwayTx' tx
    txId' = txId tx'

    anyScriptsFromTxOuts :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromTxOuts =
        Map.fromList
            [ fromLedgerToAnyScript ledgerScript
            | Just ledgerScript <- L.zipWith scriptWithHashIx
                [0..] (tx ^.. bodyTxL.outputsTxBodyL.folded)
            ]
      where
        scriptWithHashIx
            :: Word32
            -> BabbageTxOut StandardConway
            -> Maybe
                ( ScriptReference
                , Ledger.ScriptHash StandardCrypto
                , AlonzoScript StandardConway
                )
        scriptWithHashIx ix txout =
            snd (fromConwayTxOut txout) <&> \script ->
                ( ViaReferenceInput (ReferenceInput (TxIn txId' ix))
                , hashConwayScript script
                , script
                )

    anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromWits =
        Map.fromList
            [ fromLedgerToAnyScript (ViaSpending, scriptH, script)
            | (scriptH, script) <- Map.toList (tx ^. witsTxL.scriptTxWitsL)
            ]

    (assetsToMint, assetsToBurn) =
        conwayMint
            (tx ^. bodyTxL.referenceInputsTxBodyL)
            (tx ^. bodyTxL.mintTxBodyL)
            (tx ^. witsTxL)

    fromLedgerToAnyScript
        :: ( ScriptReference
           , Ledger.ScriptHash StandardCrypto
           , AlonzoScript StandardConway
           )
        -> (TokenPolicyId, AnyExplicitScript)
    fromLedgerToAnyScript (scriptRef, scriptH, script) =
        (toWalletTokenPolicyId (SL.PolicyID scriptH), toAnyScript script)
      where
        toAnyScript = \case
            Alonzo.TimelockScript timelockScript ->
                NativeExplicitScript
                    (toWalletScript (toKeyRole witCtx) timelockScript)
                    scriptRef
            Alonzo.PlutusScript ver _ ->
                PlutusExplicitScript
                    (PlutusScriptInfo
                        (toPlutusVer ver)
                        (fromLedgerScriptHash $ hashConwayScript script))
                    scriptRef

        toPlutusVer Language.PlutusV1 = PlutusVersionV1
        toPlutusVer Language.PlutusV2 = PlutusVersionV2
        toPlutusVer Language.PlutusV3 = PlutusVersionV3

    hashConwayScript = Core.hashScript @(Cardano.ShelleyLedgerEra ConwayEra)

fromConwayTx' :: Alonzo.AlonzoTx (Cardano.ShelleyLedgerEra ConwayEra) -> W.Tx
fromConwayTx' tx =
    W.Tx
        { txId = W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject conway $ Tx tx
        , fee =
            Just $ Ledger.toWalletCoin $ tx ^. bodyTxL . feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn
                <$> tx ^.. bodyTxL . inputsTxBodyL . folded
        , resolvedCollateralInputs =
            (,Nothing) . fromShelleyTxIn
                <$> tx ^.. bodyTxL . collateralInputsTxBodyL . folded
        , outputs =
            fst . fromConwayTxOut <$> tx ^.. bodyTxL . outputsTxBodyL . folded
        , collateralOutput =
            strictMaybeToMaybe $
                fst . fromConwayTxOut <$> tx ^. bodyTxL . collateralReturnTxBodyL
        , withdrawals =
            fromLedgerWithdrawals . shelleyWithdrawals $ tx
        , metadata =
            fromConwayMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Just $ case tx ^. isValidTxL of
                Alonzo.IsValid True -> W.TxScriptValid
                Alonzo.IsValid False -> W.TxScriptInvalid
        }
