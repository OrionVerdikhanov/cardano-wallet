{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Control.DeepSeq
    ( NFData
    )
import Fmt
    ( Buildable
    , GenericBuildable (GenericBuildable)
    )
import GHC.Generics
    ( Generic
    )

-- | A combination of a token policy identifier and an asset name that can be
--   used as a compound identifier.
--
data AssetId = AssetId
    { policyId
        :: !TokenPolicyId
    , assetName
        :: !AssetName
    }
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving Buildable via GenericBuildable AssetId

instance NFData AssetId
