module Cardano.Wallet.Spec.Stories.Wallet
    ( testEnvironmentIsReady
    , createdWalletListed
    , createdWalletRetrievable
    , createdWalletHasZeroAda
    , faucetWalletHasNonZeroAda
    ) where

import Cardano.Wallet.Spec.Data.AdaBalance
    ( zeroAdaBalance
    )
import Cardano.Wallet.Spec.Data.Network.Info
    ( NetworkInfo (..)
    )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..)
    )
import Cardano.Wallet.Spec.Data.Wallet
    ( Wallet (..)
    , walletId
    )
import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert
    , assert
    , assertEq
    , assertGt
    )
import Cardano.Wallet.Spec.Effect.Faucet
    ( FxFaucet
    , useFaucetMnemonic
    )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery
    , createWalletFromMnemonic
    , deleteWallet
    , getWallet
    , listKnownWallets
    , queryNetworkInfo
    )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom
    , randomMnemonic
    , randomWalletName
    )
import Cardano.Wallet.Spec.Effect.Timeout
    ( FxTimeout
    , within
    )
import Cardano.Wallet.Spec.Stories.Language
    ( FxStory
    )
import Data.Set
    ( member
    , notMember
    )
import Data.Time.TimeSpan
    ( minutes
    )

testEnvironmentIsReady :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
testEnvironmentIsReady = do
    NetworkInfo{nodeStatus} <- queryNetworkInfo
    assertEq "the Cardano Node is running and synced" nodeStatus NodeIsSynced

createdWalletListed :: FxStory fxs '[FxQuery, FxRandom, FxTimeout, FxAssert] ()
createdWalletListed = do
    wallet <- createFreshWallet
    wallets <- listKnownWallets
    assert "the new wallet is known" (wallet `member` wallets)
    within (minutes 2.0) do deleteWallet wallet
    wallets' <- listKnownWallets
    assert "the wallet is forgotten" (wallet `notMember` wallets')

createdWalletRetrievable :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
createdWalletRetrievable = do
    createdWallet <- createFreshWallet
    retrievedWallet <- getWallet (walletId createdWallet)
    assertEq "same wallet retrieved by id" createdWallet retrievedWallet

createdWalletHasZeroAda :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
createdWalletHasZeroAda = do
    wallet <- createFreshWallet
    assertEq
        "freshly created wallet has 0 ADA balance"
        (walletBalance wallet)
        zeroAdaBalance

faucetWalletHasNonZeroAda
  :: FxStory fxs '[FxQuery, FxRandom, FxFaucet, FxAssert] ()
faucetWalletHasNonZeroAda = do
    wallet <- createFaucetWallet
    assertGt
        "faucet wallet has non-zero ADA balance"
        (walletBalance wallet)
        zeroAdaBalance

--------------------------------------------------------------------------------
-- Re-usable sequences of actions ----------------------------------------------

createFreshWallet :: FxStory fxs '[FxQuery, FxRandom] Wallet
createFreshWallet = do
    name <- randomWalletName "Test Wallet"
    mnemonic <- randomMnemonic
    createWalletFromMnemonic name mnemonic

createFaucetWallet :: FxStory fxs '[FxQuery, FxRandom, FxFaucet] Wallet
createFaucetWallet = do
    name <- randomWalletName "Faucet Wallet"
    mnemonic <- useFaucetMnemonic
    createWalletFromMnemonic name mnemonic
