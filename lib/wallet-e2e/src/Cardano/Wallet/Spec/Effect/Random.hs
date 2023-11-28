{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Spec.Effect.Random where

import qualified Cardano.Mnemonic as Cardano
import qualified Cardano.Wallet.Spec.Data.Mnemonic as Mnemonic
import qualified Cardano.Wallet.Spec.Data.WalletName as WalletName
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Text as T
import qualified Effectful.State.Static.Local as State

import Cardano.Wallet.Spec.Data.Mnemonic
    ( Mnemonic
    )
import Cardano.Wallet.Spec.Data.WalletName
    ( WalletName
    )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , trace
    )
import Crypto.Encoding.BIP39
    ( toEntropy
    )
import Data.Tagged
    ( Tagged (..)
    )
import Effectful
    ( Eff
    , Effect
    , (:>)
    )
import Effectful.Dispatch.Dynamic
    ( interpret
    , reinterpret
    )
import Effectful.Fail
    ( Fail
    )
import Effectful.State.Static.Local
    ( State
    , evalState
    )
import Effectful.TH
    ( makeEffect
    )
import Prelude hiding
    ( State
    , evalState
    , get
    , trace
    )
import System.Random
    ( StdGen
    )
import System.Random.Stateful
    ( StateGenM (..)
    , uniformByteStringM
    )

data FxRandom :: Effect where
    RandomMnemonic :: FxRandom m Mnemonic
    RandomWalletName :: Tagged "Prefix" Text -> FxRandom m WalletName

$(makeEffect ''FxRandom)

runRandomMock :: Mnemonic -> (FxTrace :> es) => Eff (FxRandom : es) a -> Eff es a
runRandomMock mnemonic = interpret \_ -> \case
    RandomMnemonic -> do
        trace $ "Generated a [mock] random mnemonic: " <> Mnemonic.toText mnemonic
        pure mnemonic
    RandomWalletName (Tagged prefix) -> do
        let name = WalletName.mkUnsafe $ prefix <> "#12345"
        trace $ "Generated a random wallet name: " <> WalletName.toText name
        pure name

instance (State StdGen :> es) => MonadState StdGen (Eff es) where
    state = State.state

runRandom
    :: (FxTrace :> es, Fail :> es)
    => StdGen
    -> Eff (FxRandom : es) a
    -> Eff es a
runRandom gen = reinterpret (evalState gen) \_ -> \case
    RandomMnemonic -> do
        randomByteString <- uniformByteStringM 32 stGen
        mnemonic <-
            either
                (fail . show)
                (pure
                    . Mnemonic.unsafeFromList
                    . Cardano.mnemonicToText
                    . Cardano.entropyToMnemonic
                    )
                (toEntropy @256 randomByteString)
        trace $ "Generated a random mnemonic: " <> Mnemonic.toText mnemonic
        pure mnemonic
    RandomWalletName (Tagged prefix) -> do
        randomSuffix <- uniformByteStringM 10 stGen
        let name =
                WalletName.mkUnsafe
                    $ fold
                        [ T.stripEnd prefix
                        , " #"
                        , decodeUtf8
                            $ Base58.encodeBase58
                                Base58.bitcoinAlphabet
                                randomSuffix
                        ]
        trace $ "Generated a random wallet name: " <> WalletName.toText name
        pure name
  where
    stGen :: StateGenM StdGen = StateGenM
