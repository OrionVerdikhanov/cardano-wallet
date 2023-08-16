module Cardano.Wallet.Spec
    ( walletSpec
    ) where

import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWallet, testEnvironmentIsReady )
import Test.Syd
    ( Spec, describe, sequential )


walletSpec :: Spec
walletSpec = describe "Wallet Backend API" $ sequential do
    story "Wallet serves network info" testEnvironmentIsReady
    story "Created wallet is known" createdWallet
