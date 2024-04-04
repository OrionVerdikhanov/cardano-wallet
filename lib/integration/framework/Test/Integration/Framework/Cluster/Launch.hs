{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

{-
Control the launching of a cluster of nodes for testing purposes.
The cluster will run on the testnet network.
-}

module Test.Integration.Framework.Cluster.Launch
    ( withLocalCluster
    ) where

import Prelude

import Cardano.Launcher
    ( ProcessRun (..)
    , withBackendCreateProcess
    )
import Cardano.Launcher.Node
    ( cardanoNodeConn
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
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Exception
    ( throwIO
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
    ( withTempFile
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
    -> IO CreateProcess
localClusterProcess CommandLineOptions{..} era = do
    myEnv <- getEnvironment
    let envs =
            [ ("LOCAL_CLUSTER_ERA", clusterEraToString era)
            ]
    pure
        $ (proc "local-cluster" args)
            { env = Just $ myEnv ++ envs
            , -- , cwd = Just $ nodeDir cfg
              std_out = CreatePipe
            , std_err = CreatePipe
            }
  where
    args =
        [ "--cluster-configs"
        , pathOf clusterConfigsDir
        , "--faucet-funds"
        , pathOf faucetFundsFile
        ]
            <> case clusterDir of
                Nothing -> []
                Just clusterDir' ->
                    [ "--cluster"
                    , pathOf clusterDir'
                    ]

withLocalCluster
    :: HasCallStack
    => Int  -- ^ Port for monitoring the local cluster.
    -> Config
    -> FaucetFunds
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withLocalCluster monitoringPort Config{..} faucetFunds run = do
    r <- withTempFile $ \faucetFundsPath -> do
        saveFunds faucetFundsPath faucetFunds
        let faucetFundsFile = FileOf faucetFundsPath
            clusterConfigsDir = cfgClusterConfigs
            shelleyGenesis = pathOf cfgClusterDir </> "shelley-genesis.json"
            clusterDir = Just cfgClusterDir
        case cardanoNodeConn $ nodeSocketPath $ pathOf cfgClusterDir of
            Left err -> error $ "Failed to get socket path: " ++ err
            Right socketPath -> do
                cp <- localClusterProcess CommandLineOptions{..} cfgLastHardFork
                withBackendCreateProcess
                    (MsgLauncher "local-cluster" >$< cfgTracer)
                    cp
                    $ ProcessRun
                    $ \_ _mout _merr _ -> do
                        threadDelay 10_000_000 -- when the cluster is ready ?
                        genesisData <-
                            BS.readFile shelleyGenesis
                                >>= Aeson.throwDecodeStrict
                        let networkMagic =
                                NetworkMagic
                                    $ sgNetworkMagic genesisData
                            runningNode =
                                RunningNode
                                    { runningNodeSocketPath = socketPath
                                    , runningNodeShelleyGenesis = genesisData
                                    , runningNodeVersionData =
                                        NodeToClientVersionData
                                            { networkMagic
                                            , query = False
                                            }
                                    }
                        run runningNode
    either throwIO pure r
