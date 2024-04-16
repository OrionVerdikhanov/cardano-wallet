{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliConfigNode
    , cliRetry
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
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
    ( MonadIO (..)
    , MonadReader (..)
    )
import Data.Generics.Labels
    ()
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import Path
    ( fromAbsDir
    , fromAbsFile
    , parseAbsFile
    )
import System.IO.Temp
    ( emptyTempFile
    )

import qualified Data.Text as T

-- | Sign a transaction with all the necessary signatures.
signTx
    :: AbsFileOf "tx-body"
    -- ^ Tx body file
    -> [AbsFileOf "signing-key"]
    -- ^ Signing keys for witnesses
    -> ClusterM (AbsFileOf "tx-signed")
signTx rawTx keys = do
    Config{..} <- ask
    let outputDir = cfgClusterDir
    file <- liftIO $ emptyTempFile (fromAbsDir outputDir) "tx-signed.json"
    cli
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "sign"
          , "--tx-body-file"
          , fromAbsFile rawTx
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--out-file"
          , file
          ]
            ++ concatMap (\key -> ["--signing-key-file", fromAbsFile key]) keys
    parseAbsFile file

-- | Submit a transaction through a running node.
submitTx
    :: CardanoNodeConn
    -> Tagged "name" String
    -> AbsFileOf "tx-signed"
    -> ClusterM ()
submitTx conn name signedTx = do
    Config{..} <- ask
    cliRetry ("Submitting transaction for " <> T.pack (untag name))
        =<< cliConfigNode
            conn
            [ clusterEraToString cfgLastHardFork
            , "transaction"
            , "submit"
            , "--tx-file"
            , fromAbsFile signedTx
            , "--testnet-magic"
            , show (testnetMagicToNatural cfgTestnetMagic)
            , "--cardano-mode"
            ]

signAndSubmitTx
    :: CardanoNodeConn
    -> AbsFileOf "tx-body"
    -- ^ Tx body file
    -> [AbsFileOf "signing-key"]
    -- ^ Signing keys for witnesses
    -> Tagged "name" String
    -> ClusterM ()
signAndSubmitTx conn rawTx keys name = do
    signedTx <- signTx rawTx keys
    submitTx conn name signedTx
