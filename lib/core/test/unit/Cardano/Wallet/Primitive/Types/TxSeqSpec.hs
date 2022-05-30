{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TxSeqSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TxSeq.Gen
    ( ShrinkableTxSeq, genTxSeq, shrinkTxSeq, toTxSeq )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun (..)
    , Function (..)
    , Property
    , applyFun
    , chooseInt
    , forAll
    , label
    , property
    , scale
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Extra
    ( shrinkUniverse, shrinkWhile )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Cardano.Wallet.Primitive.Types.TxSeq as TxSeq

spec :: Spec
spec = do

    describe "genTxSeq" $ do
        it "prop_genTxSeq_isValid" $
            prop_genTxSeq_isValid & property

    describe "dropHeadTxs " $ do
        it "prop_dropHeadTxs_isValid" $
            prop_dropHeadTxs_isValid
                & withMaxSuccess 50
                & property

    describe "dropLastTxs " $ do
        it "prop_dropLastTxs_isValid" $
            prop_dropLastTxs_isValid
                & withMaxSuccess 50
                & property

    describe "mapAssetIds" $ do
        it "prop_mapAssetIds_identity" $
            prop_mapAssetIds_identity & property
        it "prop_mapAssetIds_composition" $
            prop_mapAssetIds_composition
                & withMaxSuccess 20
                & property

    describe "mapTxIds" $ do
        it "prop_mapTxIds_identity" $
            prop_mapTxIds_identity & property
        it "prop_mapTxIds_composition" $
            prop_mapTxIds_composition
                & withMaxSuccess 20
                & property

    describe "shrinkTxSeq" $ do
        it "prop_shrinkTxSeq_length" $
            prop_shrinkTxSeq_length & property
        it "prop_shrinkTxSeq_shrinkUniverse_isValid" $
            prop_shrinkTxSeq_shrinkUniverse_isValid
                & withMaxSuccess 20
                & property

    describe "shrinkAssetIds " $ do
        it "prop_shrinkAssetIds_idempotent" $
            prop_shrinkAssetIds_idempotent & property
        it "prop_shrinkAssetIds_length" $
            prop_shrinkAssetIds_length & property
        it "prop_shrinkAssetIds_isValid" $
            prop_shrinkAssetIds_isValid & property

    describe "shrinkTxIds " $ do
        it "prop_shrinkTxIds_idempotent" $
            prop_shrinkTxIds_idempotent & property
        it "prop_shrinkTxIds_length" $
            prop_shrinkTxIds_length & property
        it "prop_shrinkTxIds_isValid" $
            prop_shrinkTxIds_isValid & property

--------------------------------------------------------------------------------
-- genTxSeq
--------------------------------------------------------------------------------

prop_genTxSeq_isValid :: Property
prop_genTxSeq_isValid =
    forAll (genTxSeq genUTxO genAddress) $ \(toTxSeq -> txs) ->
        TxSeq.isValid txs

--------------------------------------------------------------------------------
-- dropHeadTxs
--------------------------------------------------------------------------------

prop_dropHeadTxs_isValid :: ShrinkableTxSeq -> Property
prop_dropHeadTxs_isValid (toTxSeq -> txs) =
    all TxSeq.isValid (TxSeq.dropHeadTxs txs) === True

--------------------------------------------------------------------------------
-- dropLastTxs
--------------------------------------------------------------------------------

prop_dropLastTxs_isValid :: ShrinkableTxSeq -> Property
prop_dropLastTxs_isValid (toTxSeq -> txs) =
    all TxSeq.isValid (TxSeq.dropLastTxs txs) === True

--------------------------------------------------------------------------------
-- mapAssetIds
--------------------------------------------------------------------------------

prop_mapAssetIds_identity :: ShrinkableTxSeq -> Property
prop_mapAssetIds_identity (toTxSeq -> txs) =
    TxSeq.mapAssetIds id txs === txs

prop_mapAssetIds_composition
    :: ShrinkableTxSeq
    -> Fun AssetId AssetId
    -> Fun AssetId AssetId
    -> Property
prop_mapAssetIds_composition
    (toTxSeq -> txs) (applyFun -> f) (applyFun -> g) =
        TxSeq.mapAssetIds f (TxSeq.mapAssetIds g txs) ===
        TxSeq.mapAssetIds (f . g) txs

--------------------------------------------------------------------------------
-- mapAssetIds
--------------------------------------------------------------------------------

prop_mapTxIds_identity :: ShrinkableTxSeq -> Property
prop_mapTxIds_identity (toTxSeq -> txs) =
    TxSeq.mapTxIds id txs === txs

prop_mapTxIds_composition
    :: ShrinkableTxSeq
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Property
prop_mapTxIds_composition
    (toTxSeq -> txs) (applyFun -> f) (applyFun -> g) =
        TxSeq.mapTxIds f (TxSeq.mapTxIds g txs) ===
        TxSeq.mapTxIds (f . g) txs

--------------------------------------------------------------------------------
-- shrinkTxSeq
--------------------------------------------------------------------------------

prop_shrinkTxSeq_length :: Property
prop_shrinkTxSeq_length =
    forAll (genTxSeq genUTxO genAddress) $ \txs ->
    forAll (chooseInt (0, TxSeq.length (toTxSeq txs))) $ \targetLength ->
    prop_inner txs targetLength
  where
    prop_inner :: ShrinkableTxSeq -> Int -> Property
    prop_inner txs targetLength =
        TxSeq.length (toTxSeq $ shrinkTxSeqToLength targetLength txs)
            === targetLength

    shrinkTxSeqToLength :: Int -> ShrinkableTxSeq -> ShrinkableTxSeq
    shrinkTxSeqToLength targetLength = shrinkTxSeq `shrinkWhile`
        ((>= targetLength) . TxSeq.length . toTxSeq)

prop_shrinkTxSeq_shrinkUniverse_isValid :: Property
prop_shrinkTxSeq_shrinkUniverse_isValid =
    forAll (scale (`div` 4) $ genTxSeq genUTxO genAddress) $ \txs ->
    prop_inner txs
  where
    prop_inner :: ShrinkableTxSeq -> Property
    prop_inner txs =
        label ("universe size: " <> show (length universe)) $
        all TxSeq.isValid (toTxSeq <$> universe)
      where
        universe = shrinkUniverse shrinkTxSeq txs

--------------------------------------------------------------------------------
-- shrinkAssetIds
--------------------------------------------------------------------------------

prop_shrinkAssetIds_idempotent :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_idempotent (toTxSeq -> txs) =
    TxSeq.assetIds (f (f txs)) === TxSeq.assetIds (f txs)
  where
    f = TxSeq.shrinkAssetIds

prop_shrinkAssetIds_length :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_length (toTxSeq -> txs) =
    length (TxSeq.assetIds (f txs)) === length (TxSeq.assetIds txs)
  where
    f = TxSeq.shrinkAssetIds

prop_shrinkAssetIds_isValid :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_isValid (toTxSeq -> txs) =
    TxSeq.isValid (TxSeq.shrinkAssetIds txs) === True

--------------------------------------------------------------------------------
-- shrinkTxIds
--------------------------------------------------------------------------------

prop_shrinkTxIds_idempotent :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_idempotent (toTxSeq -> txs) =
    TxSeq.txIds (f (f txs)) === TxSeq.txIds (f txs)
  where
    f = TxSeq.shrinkTxIds

prop_shrinkTxIds_length :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_length (toTxSeq -> txs) =
    length (TxSeq.txIds (f txs)) === length (TxSeq.txIds txs)
  where
    f = TxSeq.shrinkTxIds

prop_shrinkTxIds_isValid :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_isValid (toTxSeq -> txs) =
    TxSeq.isValid (TxSeq.shrinkTxIds txs) === True

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

deriving newtype instance Arbitrary (Hash "Tx")

instance Arbitrary ShrinkableTxSeq where
    arbitrary = genTxSeq genUTxO genAddress
    shrink = shrinkTxSeq

deriving anyclass instance CoArbitrary (Hash "TokenPolicy")
deriving anyclass instance CoArbitrary (Hash "Tx")
deriving anyclass instance CoArbitrary AssetId
deriving anyclass instance CoArbitrary TokenName
deriving anyclass instance CoArbitrary TokenPolicyId

deriving anyclass instance Function (Hash "TokenPolicy")
deriving anyclass instance Function (Hash "Tx")
deriving anyclass instance Function AssetId
deriving anyclass instance Function TokenName
deriving anyclass instance Function TokenPolicyId
