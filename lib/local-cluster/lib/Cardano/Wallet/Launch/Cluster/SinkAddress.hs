{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import Path
    ( fromAbsFile
    , relfile
    , (</>)
    )

genSinkAddress
    :: Maybe (AbsFileOf "stake-pub")
    -- ^ Stake pub
    -> ClusterM String
genSinkAddress stakePub = do
    Config{..} <- ask
    let outputDir = cfgClusterDir
    let sinkPrv = outputDir </> [relfile|sink.prv|]
    let sinkPub = outputDir </> [relfile|sink.pub|]
    cli
        [ "address"
        , "key-gen"
        , "--signing-key-file"
        , fromAbsFile sinkPrv
        , "--verification-key-file"
        , fromAbsFile sinkPub
        ]
    cliLine
        $ [ "address"
          , "build"
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--payment-verification-key-file"
          , fromAbsFile sinkPub
          ]
            ++ case stakePub of
                Nothing -> []
                Just key -> ["--stake-verification-key-file", fromAbsFile key]
