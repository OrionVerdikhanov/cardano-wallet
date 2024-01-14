{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.SinkAddress
    ( genSinkAddress
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliLine
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , TestnetMagic (testnetMagicToNatural)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import Data.Tagged
    ( Tagged
    , untag
    )
import System.FilePath
    ( (</>)
    )

genSinkAddress
    :: Tagged "output" FilePath
    -- ^ Directory to put keys
    -> Maybe (Tagged "stake-pub" FilePath)
    -- ^ Stake pub
    -> ClusterM String
genSinkAddress outputDir stakePub = do
    Config{..} <- ask
    let sinkPrv = untag outputDir </> "sink.prv"
    let sinkPub = untag outputDir </> "sink.pub"
    cli
        [ "address"
        , "key-gen"
        , "--signing-key-file"
        , sinkPrv
        , "--verification-key-file"
        , sinkPub
        ]
    cliLine
        $ [ "address"
          , "build"
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--payment-verification-key-file"
          , sinkPub
          ]
            ++ case stakePub of
                Nothing -> []
                Just key -> ["--stake-verification-key-file", untag key]
