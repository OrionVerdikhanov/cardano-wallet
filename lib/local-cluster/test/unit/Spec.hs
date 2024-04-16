module Spec where

import Prelude

import Cardano.Wallet.Faucet.Gen
    ( genFaucetFunds
    )
import Cardano.Wallet.Faucet.Yaml
    ( retrieveFunds
    , saveFunds
    )
import Path
    ( parseAbsFile
    )
import System.IO.Temp
    ( withSystemTempFile
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( forAll
    )

spec :: Spec
spec = do
    describe "FaucetFunds serialization" $ do
        it "should roundtrip" $ forAll genFaucetFunds $ \funds ->
            withSystemTempFile "funds" $ \fp _h -> do
                fp' <- parseAbsFile fp
                saveFunds fp' funds
                funds' <- retrieveFunds fp'
                funds' `shouldBe` funds
