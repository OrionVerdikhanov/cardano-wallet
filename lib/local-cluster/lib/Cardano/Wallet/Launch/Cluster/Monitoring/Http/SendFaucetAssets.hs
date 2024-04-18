{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.SendFaucetAssets
where

import Prelude

import Cardano.Wallet.Address.Encoding
    ( decodeAddress
    , encodeAddress
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (sNetworkId)
    , NetworkDiscriminant
    , SNetworkId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    , fromFlatList
    , toFlatList
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Control.Monad
    ( (>=>)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , Value
    , object
    , withObject
    , (.:)
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Bifunctor
    ( first
    )

data SendFaucetAssets (n :: NetworkDiscriminant) = SendFaucetAssets
    { batchSize :: Int
    -- ^ batch size
    , assets :: [(Address, (TokenBundle, [(String, String)]))]
    -- ^ List of addresses and the assets to send to each address
    }

instance HasSNetworkId n => ToJSON (SendFaucetAssets n) where
    toJSON SendFaucetAssets{batchSize, assets} =
        object
            [ "batchSize" .= batchSize
            , "assets" .= renderAssets (sNetworkId @n) assets
            ]

instance HasSNetworkId n => FromJSON (SendFaucetAssets n) where
    parseJSON = withObject "SendFaucetAssets" $ \o -> do
        batchSize <- o .: "batchSize"
        assets <- o .: "assets" >>= parseAssets (sNetworkId @n)
        pure SendFaucetAssets{batchSize, assets}

--- assets parsing/rendering ---------------------------------------------------

parseAssets
    :: SNetworkId n
    -> Value
    -> Parser [(Address, (TokenBundle, [(String, String)]))]
parseAssets n = parseJSON >=> mapM (parseAsset n)

parseAsset
    :: SNetworkId n
    -> Value
    -> Parser (Address, (TokenBundle, [(String, String)]))
parseAsset n = withObject "Asset" $ \o -> do
    addr <- o .: "address" >>= parseAddress n
    bundle <- o .: "bundle" >>= parseBundle
    metadata <- o .: "metadata"
    pure (addr, (bundle, metadata))

renderAssets
    :: SNetworkId n
    -> [(Address, (TokenBundle, [(String, String)]))]
    -> Value
renderAssets n = toJSON . map (renderAsset n)

renderAsset
    :: SNetworkId n
    -> (Address, (TokenBundle, [(String, String)]))
    -> Value
renderAsset n (addr, (bundle, metadata)) =
    object
        [ "address" .= renderAddress n addr
        , "bundle" .= renderBundle bundle
        , "metadata" .= metadata
        ]

-- address parsing/rendering ---------------------------------------------------

renderAddress :: SNetworkId n -> Address -> Value
renderAddress n = toJSON . encodeAddress n

parseAddress :: SNetworkId n -> Value -> Parser Address
parseAddress n x = do
    parseJSON x
        >>= eitherToParser
            . first (\e -> ShowFmt $ show (x, e))
            . decodeAddress n

eitherToParser :: Show s => Either s a -> Parser a
eitherToParser = either (fail . show) pure

--- bundle parsing/rendering ---------------------------------------------------

parseBundle :: Value -> Parser TokenBundle
parseBundle = parseJSON >=> fmap (uncurry fromFlatList) . parseBundle'

parseBundle' :: Value -> Parser (Coin, [(AssetId, TokenQuantity)])
parseBundle' = withObject "Bundle" $ \o -> do
    c <- o .: "coin" >>= parseCoin
    xs <- o .: "assets" >>= mapM parseAssetQuantity
    pure (c, xs)

parseCoin :: Value -> Parser Coin
parseCoin = fmap Coin . parseJSON

parseAssetQuantity :: Value -> Parser (AssetId, TokenQuantity)
parseAssetQuantity = withObject "AssetQuantity" $ \o -> do
    asset <- o .: "asset" >>= parseAssetId
    quantity <- o .: "quantity"
    pure (asset, quantity)

parseAssetId :: Value -> Parser AssetId
parseAssetId = withObject "AssetId" $ \o -> do
    tp <- o .: "policy"
    n <- o .: "name"
    pure $ AssetId tp n

renderBundle :: TokenBundle -> Value
renderBundle = toJSON . renderBundle' . toFlatList

renderBundle' :: (Coin, [(AssetId, TokenQuantity)]) -> Value
renderBundle' (c, xs) = object
    [ "coin" .= renderCoin c
    , "assets" .= map renderAssetQuantity xs
    ]

renderAssetQuantity :: (AssetId, TokenQuantity) -> Value
renderAssetQuantity (AssetId tp n, tq) = object
    [ "asset" .= object
        [ "policy" .= tp
        , "name" .= n
        ]
    , "quantity" .= tq
    ]

renderCoin :: Coin -> Value
renderCoin (Coin c) = toJSON c
