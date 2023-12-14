{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Provides the 'TokenMap' type, which represents a map of named non-ada
--   token quantities scoped by token policy.
--
-- The 'TokenMap' type does not provide a way to store ada quantities. If you
-- also need to store ada quantities, use the 'TokenBundle' type.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Cardano.Wallet.Primitive.Types.TokenMap as TM
--
module Cardano.Wallet.Primitive.Types.TokenMap
    (
    -- * Type
      TokenMap

    -- * Construction
    , empty
    , singleton
    , fromFlatList
    , fromNestedList
    , fromNestedMap

    -- * Deconstruction
    , toFlatList
    , toNestedList
    , toNestedMap

    -- * Filtering
    , filter

    -- * Arithmetic
    , add
    , subtract
    , difference
    , intersection

    -- * Queries
    , size

    -- * Tests
    , isEmpty
    , isNotEmpty

    -- * Quantities
    , getQuantity
    , setQuantity
    , hasQuantity
    , adjustQuantity
    , removeQuantity
    , maximumQuantity

    -- * Partitioning
    , equipartitionAssets
    , equipartitionQuantities
    , equipartitionQuantitiesWithUpperBound

    -- * Ordering
    , Lexicographic (..)

    -- * Serialization
    , Flat (..)
    , Nested (..)

    -- * Queries
    , getAssets

    -- * Transformations
    , mapAssetIds

    -- * Unsafe operations
    , unsafeSubtract

    ) where

import Prelude hiding
    ( filter
    , subtract
    )

import Algebra.PartialOrd
    ( PartialOrd (..)
    )
import Cardano.Numeric.Util
    ( equipartitionNatural
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( when
    , (<=<)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , camelTo2
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types
    ( Options (..)
    , Parser
    )
import Data.Bifunctor
    ( first
    )
import Data.Function
    ( on
    )
import Data.Hashable
    ( Hashable (..)
    , hashUsing
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Monoid.Cancellative
    ( LeftReductive
    , Reductive ((</>))
    , RightReductive
    )
import Data.Monoid.GCD
    ( GCDMonoid
    , LeftGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    , OverlappingGCDMonoid
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.Ord
    ( comparing
    )
import Data.Ratio
    ( (%)
    )
import Data.Semigroup.Commutative
    ( Commutative
    )
import Data.Set
    ( Set
    )
import Data.Text.Class
    ( toText
    )
import Fmt
    ( Buildable (..)
    , Builder
    , blockListF'
    , blockMapF
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( ErrorMessage (..)
    , TypeError
    )
import Numeric.Natural
    ( Natural
    )
import Quiet
    ( Quiet (..)
    )
import Safe
    ( fromJustNote
    )

import qualified Cardano.Wallet.Primitive.Types.AssetId as W
import qualified Cardano.Wallet.Primitive.Types.AssetName as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid.GCD as GCDMonoid
import qualified Data.Monoid.Null as MonoidNull
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A map of named token quantities, grouped by token policy.
--
-- The token map data structure has an important invariant: all token
-- quantities held within a map are non-zero.
--
-- This means that:
--
--   - using the 'setQuantity' function to add a zero-valued quantity to a
--     map is equivalent to applying the identity operation to that map.
--
--   - using the 'setQuantity' function to change an existing quantity to zero
--     is equivalent to removing that quantity from the map.
--
-- As a consequence of this invariant, the token map data structure is
-- always in its canonical form: we can perform an equality check without
-- needing any extra canonicalization steps.
--
newtype TokenMapF c = TokenMap
    { unTokenMap
        :: MonoidMap (PolicyId c) (MonoidMap (AssetName c) TokenQuantity)
    }
    deriving stock Generic

class
    ( ContextType (AssetId c)
    , ContextType (AssetName c)
    , ContextType (PolicyId c)
    ) =>
    Context c
  where
    type AssetId c = assetId | assetId -> c
    type AssetName c
    type PolicyId c
    mkAssetId :: (PolicyId c, AssetName c) -> AssetId c
    unAssetId :: AssetId c -> (PolicyId c, AssetName c)

type ContextType a = (Buildable a, Hashable a, NFData a, Ord a, Read a, Show a)

data StandardContext

instance Context StandardContext where
    type AssetId StandardContext = W.AssetId
    type AssetName StandardContext = W.AssetName
    type PolicyId StandardContext = W.TokenPolicyId
    mkAssetId (policyId, assetName) = W.AssetId policyId assetName
    unAssetId (W.AssetId policyId assetName) = (policyId, assetName)

type TokenMap = TokenMapF StandardContext

deriving via Quiet (TokenMapF c) instance Context c => Read (TokenMapF c)
deriving via Quiet (TokenMapF c) instance Context c => Show (TokenMapF c)

deriving newtype instance Context c => Eq                   (TokenMapF c)
deriving newtype instance Context c => Semigroup            (TokenMapF c)
deriving newtype instance Context c => Commutative          (TokenMapF c)
deriving newtype instance Context c => Monoid               (TokenMapF c)
deriving newtype instance Context c => MonoidNull           (TokenMapF c)
deriving newtype instance Context c => LeftReductive        (TokenMapF c)
deriving newtype instance Context c => RightReductive       (TokenMapF c)
deriving newtype instance Context c => Reductive            (TokenMapF c)
deriving newtype instance Context c => LeftGCDMonoid        (TokenMapF c)
deriving newtype instance Context c => RightGCDMonoid       (TokenMapF c)
deriving newtype instance Context c => GCDMonoid            (TokenMapF c)
deriving newtype instance Context c => OverlappingGCDMonoid (TokenMapF c)
deriving newtype instance Context c => Monus                (TokenMapF c)

deriving instance Context c => NFData (TokenMapF c)

instance Context c => Hashable (TokenMapF c) where
    hashWithSalt = hashUsing toNestedList

--------------------------------------------------------------------------------
-- Ordering
--------------------------------------------------------------------------------

-- | Token maps can be partially ordered, but there is no total ordering of
--   token maps that's consistent with their arithmetic properties.
--
-- In the event that someone attempts to define an 'Ord' instance for the
-- 'TokenMap' type, we generate a type error.
--
-- If some arbitrary ordering is needed (for example, so that token maps can
-- be included in an ordered set), the recommended course of action is to
-- define a newtype with its own dedicated 'Ord' instance.
--
instance
    ( Context c
    , TypeError ('Text "Ord not supported for token maps")
    ) =>
    Ord (TokenMapF c)
  where
    compare = error "Ord not supported for token maps"

-- | Partial ordering for token maps.
--
-- There is no total ordering of token maps that's consistent with their
-- arithmetic properties.
--
-- To see why this is true, consider how we might order the following maps:
--
--     >>> p = fromFlatList [(assetA, 2), (assetB, 1)]
--     >>> q = fromFlatList [(assetA, 1), (assetB, 2)]
--
-- One possibility would be to use a lexicographic ordering, but this is not
-- arithmetically useful.
--
-- Instead, we define a partial order, where map 'x' is less than or equal
-- to map 'y' if (and only if):
--
--     - all the quantities in map 'x' are less than or equal to their
--       corresponding quantities in map 'y';
--
--     - all the quantities in map 'y' are greater than or equal to their
--       corresponding quantities in map 'x'.
--
-- For example, consider the following pair of maps:
--
--     >>> x = fromFlatList [(assetA, 1)]
--     >>> y = fromFlatList [(assetA, 2), (assetB, 1)]
--
-- In the above example, map 'x' is strictly less than map 'y'.
--
instance Context c => PartialOrd (TokenMapF c) where
    leq = MonoidMap.isSubmapOf `on` unTokenMap

-- | Defines a lexicographic ordering.
--
newtype Lexicographic a = Lexicographic {unLexicographic :: a}
    deriving (Eq, Show)

instance Context c => Ord (Lexicographic (TokenMapF c)) where
    compare = comparing (toNestedList . unLexicographic)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

-- | When used with the 'Buildable' or 'ToJSON' instances, provides a flat
-- serialization style, where token quantities are paired with their asset
-- identifiers.
--
newtype Flat a = Flat { getFlat :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Flat a))

-- | When used with the 'Buildable' or 'ToJSON' instances, provides a nested
-- serialization style, where token quantities are grouped by policy
-- identifier.
--
newtype Nested a = Nested { getNested :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Nested a))

--------------------------------------------------------------------------------
-- Text serialization
--------------------------------------------------------------------------------

instance Context c => Buildable (Flat (TokenMapF c)) where
    build = buildTokenMap . getFlat
      where
        buildTokenMap =
            buildList buildAssetQuantity . toFlatList
        buildAssetQuantity (unAssetId -> (policyId, assetName), quantity) =
            buildMap
            [ ("policyId",
                build policyId)
            , ("assetName",
                build assetName)
            , ("quantity",
                build quantity)
            ]

instance Context c => Buildable (Nested (TokenMapF c))
  where
    build = buildTokenMap . unTokenMap . getNested
      where
        buildTokenMap =
            buildList buildPolicy . MonoidMap.toList
        buildPolicy (policyId, assetMap) = buildMap
            [ ("policyId",
                build policyId)
            , ("tokens",
                buildList buildTokenQuantity (MonoidMap.toList assetMap))
            ]
        buildTokenQuantity (assetName, quantity) = buildMap
            [ ("assetName",
                build assetName)
            , ("quantity",
                build quantity)
            ]

buildList :: Foldable f => (a -> Builder) -> f a -> Builder
buildList = blockListF' "-"

buildMap :: [(String, Builder)] -> Builder
buildMap = blockMapF . fmap (first $ id @String)

--------------------------------------------------------------------------------
-- JSON serialization (common)
--------------------------------------------------------------------------------

jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_') }

jsonFailWith :: String -> Parser a
jsonFailWith s = fail $
    "Error while deserializing token map from JSON: " <> s <> "."

jsonFailWithEmptyTokenList :: W.TokenPolicyId -> Parser a
jsonFailWithEmptyTokenList policyId = jsonFailWith $ unwords
    [ "Encountered empty token list for policy"
    , show (toText policyId)
    ]

jsonFailWithZeroValueTokenQuantity :: W.TokenPolicyId -> W.AssetName -> Parser a
jsonFailWithZeroValueTokenQuantity policyId assetName = jsonFailWith $ unwords
    [ "Encountered zero-valued quantity for token"
    , show (toText assetName)
    , "within policy"
    , show (toText policyId)
    ]

--------------------------------------------------------------------------------
-- JSON serialization (flat)
--------------------------------------------------------------------------------

instance ToJSON (Flat TokenMap) where
    toJSON = toJSON . fmap fromTuple . toFlatList . getFlat
      where
        fromTuple (W.AssetId policyId assetName, quantity) =
            FlatAssetQuantity policyId assetName quantity

instance FromJSON (Flat TokenMap) where
    parseJSON =
        fmap (Flat . fromFlatList) . mapM parseTuple <=< parseJSON
      where
        parseTuple :: FlatAssetQuantity -> Parser (W.AssetId, TokenQuantity)
        parseTuple (FlatAssetQuantity policyId assetName quantity) = do
            when (TokenQuantity.isZero quantity) $
                jsonFailWithZeroValueTokenQuantity policyId assetName
            pure (W.AssetId policyId assetName, quantity)

-- Used for JSON serialization only: not exported.
data FlatAssetQuantity = FlatAssetQuantity
    { _policyId :: !W.TokenPolicyId
    , _assetName :: !W.AssetName
    , _quantity :: !TokenQuantity
    } deriving Generic

instance FromJSON FlatAssetQuantity where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON FlatAssetQuantity where
    toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- JSON serialization (nested)
--------------------------------------------------------------------------------

instance ToJSON (Nested TokenMap) where
    toJSON = toJSON . fmap mapOuter . toNestedList . getNested
      where
        mapOuter = uncurry NestedMapEntry . fmap mapInner
        mapInner = NE.toList . fmap (uncurry NestedTokenQuantity)

instance FromJSON (Nested TokenMap) where
    parseJSON = parseEntryList <=< parseJSON @[NestedMapEntry]
      where
        parseEntryList :: [NestedMapEntry] -> Parser (Nested TokenMap)
        parseEntryList = fmap (Nested . fromNestedList) . mapM parseEntry

        parseEntry
            :: NestedMapEntry
            -> Parser (W.TokenPolicyId, NonEmpty (W.AssetName, TokenQuantity))
        parseEntry (NestedMapEntry policyId mTokens) = do
            tokens <- maybe (jsonFailWithEmptyTokenList policyId) pure $
                NE.nonEmpty mTokens
            (policyId,) <$> mapM (parseToken policyId) tokens

        parseToken
            :: W.TokenPolicyId
            -> NestedTokenQuantity
            -> Parser (W.AssetName, TokenQuantity)
        parseToken policyId (NestedTokenQuantity assetName quantity) = do
            when (TokenQuantity.isZero quantity) $
                jsonFailWithZeroValueTokenQuantity policyId assetName
            pure (assetName, quantity)

-- Used for JSON serialization only: not exported.
data NestedMapEntry = NestedMapEntry
    { _policyId :: !W.TokenPolicyId
    , _tokens :: ![NestedTokenQuantity]
    } deriving Generic

-- Used for JSON serialization only: not exported.
data NestedTokenQuantity = NestedTokenQuantity
    { _assetName :: !W.AssetName
    , _quantity :: !TokenQuantity
    } deriving Generic

instance FromJSON NestedMapEntry where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON NestedMapEntry where
    toJSON = genericToJSON jsonOptions

instance FromJSON NestedTokenQuantity where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON NestedTokenQuantity where
    toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | The empty token map.
--
empty :: Context c => TokenMapF c
empty = TokenMap mempty

-- | Creates a singleton token map with just one token quantity.
--
-- If the specified token quantity is zero, then the resultant map will be
-- equal to the 'empty' map.
--
singleton :: Context c => AssetId c -> TokenQuantity -> TokenMapF c
singleton = setQuantity empty

-- | Creates a token map from a flat list.
--
-- If an asset name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant map.
--
fromFlatList :: Context c => [(AssetId c, TokenQuantity)] -> TokenMapF c
fromFlatList = F.foldl' acc empty
  where
    acc b (asset, quantity) = adjustQuantity b asset (<> quantity)

-- | Creates a token map from a nested list.
--
-- If an asset name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant map.
--
fromNestedList
    :: Context c
    => [(PolicyId c, NonEmpty (AssetName c, TokenQuantity))]
    -> TokenMapF c
fromNestedList entries = fromFlatList
    [ (mkAssetId (policyId, assetName), quantity)
    | (policyId, tokenQuantities) <- entries
    , (assetName, quantity) <- NE.toList tokenQuantities
    ]

-- | Creates a token map from a nested map.
--
fromNestedMap
    :: Context c
    => Map (PolicyId c) (Map (AssetName c) TokenQuantity)
    -> TokenMapF c
fromNestedMap = TokenMap . MonoidMap.fromMap . fmap MonoidMap.fromMap

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a token map to a flat list.
--
toFlatList :: Context c => TokenMapF c -> [(AssetId c, TokenQuantity)]
toFlatList b =
    [ (mkAssetId (policyId, assetName), quantity)
    | (policyId, tokenQuantities) <- toNestedList b
    , (assetName, quantity) <- NE.toList tokenQuantities
    ]

-- | Converts a token map to a nested list.
--
toNestedList
    :: TokenMapF c
    -> [(PolicyId c, NonEmpty (AssetName c, TokenQuantity))]
toNestedList (TokenMap m) =
    mapMaybe (traverse (NE.nonEmpty . MonoidMap.toList)) $ MonoidMap.toList m

-- | Converts a token map to a nested map.
--
toNestedMap :: TokenMapF c -> Map (PolicyId c) (Map (AssetName c) TokenQuantity)
toNestedMap (TokenMap m) = MonoidMap.toMap <$> MonoidMap.toMap m

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

filter :: Context c => (AssetId c -> Bool) -> TokenMapF c -> TokenMapF c
filter f = fromFlatList . L.filter (f . fst) . toFlatList

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

-- | Adds one token map to another.
--
add :: Context c => TokenMapF c -> TokenMapF c -> TokenMapF c
add = (<>)

-- | Subtracts the second token map from the first.
--
-- Returns 'Nothing' if the second map is not less than or equal to the first
-- map when compared with the `leq` function.
--
subtract :: Context c => TokenMapF c -> TokenMapF c -> Maybe (TokenMapF c)
subtract = (</>)

-- | Analogous to @Set.difference@, return the difference between two token
-- maps.
--
-- The following property holds:
-- prop> x `leq` (x `difference` y) `add` y
--
-- Note that there's a `leq` rather than equality, which we'd expect if this was
-- subtraction of integers. I.e.
--
-- >>> (0 - 1) + 1
-- 0
--
-- whereas
--
-- >>> let oneToken = singleton aid (TokenQuantity 1)
-- >>> (mempty `difference` oneToken) `add` oneToken
-- oneToken
--
difference :: Context c => TokenMapF c -> TokenMapF c -> TokenMapF c
difference = (<\>)

-- | Computes the intersection of two token maps.
--
-- Analogous to @Set.intersection@.
--
-- Example:
--
-- >>> m1 = [("a", 1), ("b", 2), ("c", 3)          ]
-- >>> m2 = [          ("b", 3), ("c", 2), ("d", 1)]
-- >>> intersection m1 m2
--          [          ("b", 2), ("c", 2)          ]
--
intersection :: Context c => TokenMapF c -> TokenMapF c -> TokenMapF c
intersection = GCDMonoid.gcd

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Returns the number of unique assets in a token map.
--
size :: Context c => TokenMapF c -> Int
size = Set.size . getAssets

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- | Returns true if and only if the given map is empty.
--
isEmpty :: Context c => TokenMapF c -> Bool
isEmpty = MonoidNull.null

-- | Returns true if and only if the given map is not empty.
--
isNotEmpty :: Context c => TokenMapF c -> Bool
isNotEmpty = not . MonoidNull.null

--------------------------------------------------------------------------------
-- Quantities
--------------------------------------------------------------------------------

-- | Gets the quantity associated with a given asset.
--
-- If the given map does not have an entry for the specified asset, this
-- function returns a value of zero.
--
getQuantity :: Context c => TokenMapF c -> AssetId c -> TokenQuantity
getQuantity (TokenMap m) (unAssetId -> (policyId, assetName)) =
    MonoidMap.get assetName (MonoidMap.get policyId m)

-- | Updates the quantity associated with a given asset.
--
-- If the given quantity is zero, the resultant map will not have an entry for
-- the given asset.
--
setQuantity
    :: Context c
    => TokenMapF c
    -> AssetId c
    -> TokenQuantity
    -> TokenMapF c
setQuantity (TokenMap m) (unAssetId -> (policyId, assetName)) quantity =
    TokenMap $ MonoidMap.adjust (MonoidMap.set assetName quantity) policyId m

-- | Returns true if and only if the given map has a non-zero quantity for the
--   given asset.
--
hasQuantity :: Context c => TokenMapF c -> AssetId c -> Bool
hasQuantity m = not . MonoidNull.null . getQuantity m

-- | Uses the specified function to adjust the quantity associated with a
--   given asset.
--
-- If the result of adjusting the quantity is equal to zero, the resultant map
-- will not have an entry for the given asset.
--
adjustQuantity
    :: Context c
    => TokenMapF c
    -> AssetId c
    -> (TokenQuantity -> TokenQuantity)
    -> TokenMapF c
adjustQuantity (TokenMap m) (unAssetId -> (policyId, assetName)) f =
    TokenMap $ MonoidMap.adjust (MonoidMap.adjust f assetName) policyId m

-- | Removes the quantity associated with the given asset.
--
-- This is equivalent to calling 'setQuantity' with a value of zero.
--
removeQuantity :: Context c => TokenMapF c -> AssetId c -> TokenMapF c
removeQuantity m asset = setQuantity m asset TokenQuantity.zero

-- | Get the largest quantity from this map.
--
maximumQuantity :: TokenMapF c -> TokenQuantity
maximumQuantity = F.foldl' (F.foldl' max) mempty . unTokenMap

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

-- | Partitions a token map into 'n' smaller maps, where the asset sets of the
--   resultant maps are disjoint.
--
-- In the resultant maps, the smallest asset set size and largest asset set
-- size will differ by no more than 1.
--
-- The quantities of each asset are unchanged.
--
equipartitionAssets
    :: Context c
    => TokenMapF c
    -- ^ The token map to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the token map.
    -> NonEmpty (TokenMapF c)
    -- ^ The partitioned maps.
equipartitionAssets m mapCount =
    fromFlatList <$> NE.unfoldr generateChunk (assetCounts, toFlatList m)
  where
    -- The total number of assets.
    assetCount :: Int
    assetCount = Set.size $ getAssets m

    -- How many asset quantities to include in each chunk.
    assetCounts :: NonEmpty Int
    assetCounts = fromIntegral @Natural @Int <$>
        equipartitionNatural (fromIntegral @Int @Natural assetCount) mapCount

    -- Generates a single chunk of asset quantities.
    generateChunk :: (NonEmpty Int, [aq]) -> ([aq], Maybe (NonEmpty Int, [aq]))
    generateChunk (c :| mcs, aqs) = case NE.nonEmpty mcs of
        Just cs -> (prefix, Just (cs, suffix))
        Nothing -> (aqs, Nothing)
      where
        (prefix, suffix) = L.splitAt c aqs

-- | Partitions a token map into 'n' smaller maps, where the quantity of each
--   token is equipartitioned across the resultant maps.
--
-- In the resultant maps, the smallest quantity and largest quantity of a given
-- token will differ by no more than 1.
--
-- The resultant list is sorted into ascending order when maps are compared
-- with the 'leq' function.
--
equipartitionQuantities
    :: Context c
    => TokenMapF c
    -- ^ The map to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the map.
    -> NonEmpty (TokenMapF c)
    -- ^ The partitioned maps.
equipartitionQuantities m count =
    F.foldl' accumulate (empty <$ count) (toFlatList m)
  where
    accumulate
        :: Context c
        => NonEmpty (TokenMapF c)
        -> (AssetId c, TokenQuantity)
        -> NonEmpty (TokenMapF c)
    accumulate maps (asset, quantity) = NE.zipWith (<>) maps $
        singleton asset <$>
            TokenQuantity.equipartition quantity count

-- | Partitions a token map into 'n' smaller maps, where the quantity of each
--   token is equipartitioned across the resultant maps, with the goal that no
--   token quantity in any of the resultant maps exceeds the given upper bound.
--
-- The value 'n' is computed automatically, and is the minimum value required
-- to achieve the goal that no token quantity in any of the resulting maps
-- exceeds the maximum allowable token quantity.
--
equipartitionQuantitiesWithUpperBound
    :: Context c
    => TokenMapF c
    -> TokenQuantity
    -- ^ Maximum allowable token quantity.
    -> NonEmpty (TokenMapF c)
    -- ^ The partitioned maps.
equipartitionQuantitiesWithUpperBound m (TokenQuantity maxQuantity)
    | maxQuantity == 0 =
        maxQuantityZeroError
    | currentMaxQuantity <= maxQuantity =
        m :| []
    | otherwise =
        equipartitionQuantities m (() :| replicate extraPartCount ())
  where
    TokenQuantity currentMaxQuantity = maximumQuantity m

    extraPartCount :: Int
    extraPartCount = floor $ pred currentMaxQuantity % maxQuantity

    maxQuantityZeroError = error $ unwords
        [ "equipartitionQuantitiesWithUpperBound:"
        , "the maximum allowable token quantity cannot be zero."
        ]

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

getAssets :: Context c => TokenMapF c -> Set (AssetId c)
getAssets = Set.fromList . fmap fst . toFlatList

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

mapAssetIds
    :: Context c
    => (AssetId c -> AssetId c)
    -> TokenMapF c
    -> TokenMapF c
mapAssetIds f m = fromFlatList $ first f <$> toFlatList m

--------------------------------------------------------------------------------
-- Unsafe operations
--------------------------------------------------------------------------------

-- | Subtracts the second token map from the first.
--
-- Pre-condition: the second map is less than or equal to the first map when
-- compared with the `leq` function.
--
-- Throws a run-time exception if the pre-condition is violated.
--
unsafeSubtract :: Context c => TokenMapF c -> TokenMapF c -> TokenMapF c
unsafeSubtract b1 b2 = fromJustNote "TokenMap.unsafeSubtract" $ b1 </> b2
