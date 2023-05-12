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
-- | A singleton type to capture the flavor of a state.
data WalletFlavorS s n where
    ShelleyWallet :: WalletFlavorS (SeqState n ShelleyKey) n
    IcarusWallet :: WalletFlavorS (SeqState n IcarusKey) n
    ByronWallet :: WalletFlavorS (RndState n) n
    SharedWallet :: WalletFlavorS (SharedState n SharedKey) n
    BenchByronWallet :: WalletFlavorS (RndAnyState n p) n
    BenchShelleyWallet :: WalletFlavorS (SeqAnyState n ShelleyKey p) n

-- | A function to reify the flavor of a state.
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

-- | A type for states that will be used in tests.
newtype TestState s (k :: (Depth -> Type -> Type)) = TestState s
    deriving (Generic, Show, Eq)

-- | A type family to get the key type from a state.
type family KeyOf (s :: Type) :: (Depth -> Type -> Type) where
    KeyOf (SeqState n k) = k
    KeyOf (RndState n) = ByronKey
    KeyOf (SharedState n k) = k
    KeyOf (SeqAnyState n k p) = k
    KeyOf (RndAnyState n p) = ByronKey
    KeyOf (TestState s k) = k

-- | A singleton type to capture the flavor of a key.
data KeyFlavorS a where
    ByronKeyS :: KeyFlavorS ByronKey
    IcarusKeyS :: KeyFlavorS IcarusKey
    ShelleyKeyS :: KeyFlavorS ShelleyKey
    SharedKeyS :: KeyFlavorS SharedKey

-- | A function to reify the flavor of a key.
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

-- | Constraints for a state with any key implied.
type StateWithAnyKey s = KeyFlavor (KeyOf s)

-- | A function to reify the flavor of a key from a state type.
--
-- use with
-- > keyFlavorOfState @s
keyFlavorOfState :: StateWithAnyKey s => KeyFlavorS (KeyOf s)
keyFlavorOfState = keyFlavor

-- | Constraints for a state with a specific key.
type StateWithKey s k = (StateWithAnyKey s, KeyOf s ~ k)
