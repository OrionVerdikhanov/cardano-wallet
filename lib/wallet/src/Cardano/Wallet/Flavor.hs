{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Flavor
    ( WalletFlavorS (..)
    , WalletFlavor (..)
    , KeyOf
    , TestState (..)
    , KeyFlavor (..)
    , KeyFlavorS (..)
    , StateWithAnyKey
    , keyFlavorOfState
    , StateWithKey
    )
where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndAnyState, RndState (..) )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqAnyState, SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState (..) )
import Data.Kind
    ( Type )
import GHC.Generics
    ( Generic )

data WalletFlavorS s n where
    ShelleyWallet :: WalletFlavorS (SeqState n ShelleyKey) n
    IcarusWallet :: WalletFlavorS (SeqState n IcarusKey) n
    ByronWallet :: WalletFlavorS (RndState n) n
    SharedWallet :: WalletFlavorS (SharedState n SharedKey) n
    BenchByronWallet :: WalletFlavorS (RndAnyState n p) n
    BenchShelleyWallet :: WalletFlavorS (SeqAnyState n ShelleyKey p) n

class WalletFlavor s n where
    walletFlavor :: WalletFlavorS s n

instance WalletFlavor (SeqState n IcarusKey) n where
    walletFlavor = IcarusWallet

instance WalletFlavor (SeqState n ShelleyKey) n where
    walletFlavor = ShelleyWallet

instance WalletFlavor (RndState n) n where
    walletFlavor = ByronWallet

instance WalletFlavor (SeqAnyState n ShelleyKey p) n where
    walletFlavor = BenchShelleyWallet

instance WalletFlavor (RndAnyState n p) n where
    walletFlavor = BenchByronWallet

instance WalletFlavor (SharedState n SharedKey) n where
    walletFlavor = SharedWallet

newtype TestState s (k :: (Depth -> Type -> Type)) = TestState s
    deriving (Generic, Show, Eq)

type family KeyOf (s :: Type) :: (Depth -> Type -> Type) where
    KeyOf (SeqState n k) = k
    KeyOf (RndState n) = ByronKey
    KeyOf (SharedState n k) = k
    KeyOf (SeqAnyState n k p) = k
    KeyOf (RndAnyState n p) = ByronKey
    KeyOf (TestState s k) = k

data KeyFlavorS a where
    ByronKeyS :: KeyFlavorS ByronKey
    IcarusKeyS :: KeyFlavorS IcarusKey
    ShelleyKeyS :: KeyFlavorS ShelleyKey
    SharedKeyS :: KeyFlavorS SharedKey

class KeyFlavor a where
    keyFlavor :: KeyFlavorS a

instance KeyFlavor ByronKey where
    keyFlavor = ByronKeyS

instance KeyFlavor IcarusKey where
    keyFlavor = IcarusKeyS

instance KeyFlavor ShelleyKey where
    keyFlavor = ShelleyKeyS

instance KeyFlavor SharedKey where
    keyFlavor = SharedKeyS

type StateWithAnyKey s = KeyFlavor (KeyOf s)

keyFlavorOfState :: StateWithAnyKey s => KeyFlavorS (KeyOf s)
keyFlavorOfState = keyFlavor

type StateWithKey s k = (StateWithAnyKey s, KeyOf s ~ k)
