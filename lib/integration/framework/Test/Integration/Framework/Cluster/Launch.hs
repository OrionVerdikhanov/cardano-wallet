{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-
Control the launching of a cluster of nodes for testing purposes.
The cluster will run on the testnet network.
-}

module Test.Integration.Framework.Cluster.Launch
    ( withLocalCluster
    ) where

import Prelude

import Cardano.Launcher
    ( withBackendCreateProcess
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , cardanoNodeConn
    , nodeSocketPath
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (sgNetworkMagic)
    )
import Cardano.Wallet.Faucet.Yaml
    ( saveFunds
    )
import Cardano.Wallet.Launch.Cluster
    ( ClusterEra
    , ClusterLog (MsgLauncher)
    , FaucetFunds
    , FileOf (..)
    , RunningNode
    , clusterEraToString
    , pathOf
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , Monitoring
    , renderMonitoring
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , NodePathSegment (pathOfNodePathSegment)
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( void
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Trans
    ( lift
    )
import Data.Aeson
    ( FromJSON
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import GHC.Stack
    ( HasCallStack
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..)
    )
import System.Environment
    ( getEnvironment
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Extra
    ( IOMode (..)
    , withFile
    , withTempFile
    )
import System.Process.Extra
    ( CreateProcess (..)
    , StdStream (..)
    , proc
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS

localClusterProcess
    :: CommandLineOptions
    -> ClusterEra
    -> ContT r IO CreateProcess
localClusterProcess CommandLineOptions{..} era = do
    myEnv <- lift getEnvironment

    let envs =
            [ ("LOCAL_CLUSTER_ERA", clusterEraToString era)
            ]
    output <- case clusterLogs of
        Nothing -> pure Inherit
        Just (FileOf logFile) ->
            fmap UseHandle
                $ ContT
                $ withFile logFile WriteMode
    pure
        $ (proc "local-cluster" args)
            { env = Just $ myEnv ++ envs
            , -- , cwd = Just $ nodeDir cfg
              std_out = output
            , std_err = output
            }
  where
    args =
        renderMonitoring monitoring
        <> [ "--cluster-configs"
        , pathOf clusterConfigsDir
        , "--faucet-funds"
        , pathOf faucetFundsFile
        ] <>
            case clusterDir of
                Nothing -> []
                Just clusterDir' ->
                    [ "--cluster"
                    , pathOf clusterDir'
                    ]

withFaucetFunds
    :: HasCallStack
    => FaucetFunds
    -> ContT r IO (FileOf s)
withFaucetFunds faucetFunds = ContT $ \action ->
    withTempFile $ \faucetFundsPath -> do
        saveFunds faucetFundsPath faucetFunds
        action $ FileOf faucetFundsPath

withSocketPath
    :: HasCallStack
    => FileOf s
    -> ContT r m CardanoNodeConn
withSocketPath cfgClusterDir = ContT $ \f ->
    case cardanoNodeConn $ nodeSocketPath $ pathOf cfgClusterDir of
        Left err -> error $ "Failed to get socket path: " ++ err
        Right socketPath -> f socketPath

withGenesisData :: FromJSON a => FilePath -> ContT r IO a
withGenesisData shelleyGenesis = ContT $ \f -> do
    genesisData <- BS.readFile shelleyGenesis >>= Aeson.throwDecodeStrict
    f genesisData

withLocalCluster
    :: HasCallStack
    => Maybe Monitoring
    -- ^ If to monitor the cluster.
    -> Config
    -- ^ Configuration for the cluster.
    -> FaucetFunds
    -- ^ Initial faucet funds.
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withLocalCluster
    monitoring
    Config{..}
    faucetFunds
    action = do
        let
            clusterConfigsDir = cfgClusterConfigs
            relayDir =
                pathOf cfgClusterDir
                    </> pathOfNodePathSegment cfgRelayNodePath
            shelleyGenesis = pathOf cfgClusterDir </> "shelley-genesis.json"
            clusterDir = Just cfgClusterDir
            clusterLogs = cfgClusterLogFile
        evalContT $ do
            faucetFundsFile <- withFaucetFunds faucetFunds
            socketPath <- withSocketPath $ FileOf relayDir
            cp <- localClusterProcess CommandLineOptions{..} cfgLastHardFork
            void
                $ ContT
                $ withBackendCreateProcess
                    (MsgLauncher "local-cluster" >$< cfgTracer)
                    cp
            lift $ threadDelay 10_000_000 -- when the cluster is ready ?
            genesisData <- withGenesisData shelleyGenesis
            lift
                $ action
                $ RunningNode
                    { runningNodeSocketPath = socketPath
                    , runningNodeShelleyGenesis = genesisData
                    , runningNodeVersionData =
                        NodeToClientVersionData
                            { networkMagic =
                                NetworkMagic
                                    $ sgNetworkMagic genesisData
                            , query = False
                            }
                    }
