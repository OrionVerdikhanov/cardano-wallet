{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Prelude

import Cardano.BM.Tracing
    ( Severity (..)
    , traceWith
    )
import Cardano.Startup
    ( installSignalHandlers
    , setDefaultFilePermissions
    )
import Cardano.Wallet.Faucet.Yaml
    ( retrieveFunds
    )
import Cardano.Wallet.Launch.Cluster
    ( Config (..)
    , TestnetMagic (..)
    , clusterEraFromEnv
    , clusterEraToString
    , defaultPoolConfigs
    , logFileConfigFromEnv
    , runningNodeSocketPath
    , withCluster
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , parseCommandLineOptions
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , filteredLogRenderer
    , threadSafeTracer
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Monitor
    ( CardanoNodeConnVar (writeCardanoNodeConnVar)
    , mkCardanoNodeConnVar
    , withMonitoring
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Lens
    ( over
    )
import Control.Monad
    ( (<=<)
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Trans
    ( lift
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Main.Utf8
    ( withUtf8
    )
import Path
    ( parseAbsDir
    , reldir
    )
import System.Environment.Extended
    ( isEnvSet
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import UnliftIO
    ( stdout
    )

-- |
-- # OVERVIEW
--
-- This starts a cluster of Cardano nodes with:
--
-- - 1 relay node
-- - 1 BFT leader
-- - 4 stake pools
--
-- The BFT leader and pools are all fully connected. The network starts in the
-- Byron Era and transitions into the Shelley era. Once in the Shelley era and
-- once pools are registered and up-and-running, an instance of cardano-wallet
-- is started.
--
-- Pools have slightly different settings summarized in the table below:
--
-- | #       | Pledge | Retirement      | Metadata       |
-- | ---     | ---    | ---             | ---            |
-- | Pool #0 | 2M Ada | Never           | Genesis Pool A |
-- | Pool #1 | 1M Ada | Epoch 3         | Genesis Pool B |
-- | Pool #2 | 1M Ada | Epoch 100_000   | Genesis Pool C |
-- | Pool #3 | 1M Ada | Epoch 1_000_000 | Genesis Pool D |
--
-- Pools' metadata are hosted on static local servers started alongside pools.
--
-- # CONFIGURATION
--
-- There are several environment variables that can be set to make debugging
-- easier if needed:
--
-- - CARDANO_WALLET_PORT  (default: random)
--     choose a port for the API to listen on
--
-- - CARDANO_NODE_TRACING_MIN_SEVERITY  (default: Info)
--     increase or decrease the logging severity of the nodes.
--
-- - CARDANO_WALLET_TRACING_MIN_SEVERITY  (default: Info)
--     increase or decrease the logging severity of cardano-wallet.
--
-- - TESTS_TRACING_MIN_SEVERITY  (default: Notice)
--     increase or decrease the logging severity of the test cluster framework.
--
-- - LOCAL_CLUSTER_ERA  (default: Mary)
--     By default, the cluster will start in the latest era by enabling
--     "virtual hard forks" in the node config files.
--     The final era can be changed with this variable.
--
-- - TOKEN_METADATA_SERVER  (default: none)
--     Use this URL for the token metadata server.
--
-- - NO_CLEANUP  (default: temp files are cleaned up)
--     If set, the temporary directory used as a state directory for
--     nodes and wallet data won't be cleaned up.
testnetMonitorAPI :: Proxy (API ('Testnet 42))
testnetMonitorAPI = Proxy

main :: IO ()
main = withUtf8 $ do
    -- Handle SIGTERM properly
    installSignalHandlers (putStrLn "Terminated")

    -- Ensure key files have correct permissions for cardano-cli
    setDefaultFilePermissions

    logTracer <- threadSafeTracer stdout $ filteredLogRenderer Debug
    clusterEra <- clusterEraFromEnv
    cfgNodeLogging <-
        logFileConfigFromEnv
            $ Just
            $ clusterEraToString clusterEra
    o@CommandLineOptions
        { clusterConfigsDir
        , faucetFundsFile
        , clusterDir
        , monitoring
        , clusterLogs
        , clusterControl
        , nodeToClientSocket
        } <-
        parseCommandLineOptions
    traceWith logTracer $ MsgCommandLineOptions o
    evalContT $ do
        clusterPath <-
            case clusterDir of
                Just path -> pure path
                Nothing -> do
                    skipCleanup <- lift $ SkipCleanup <$> isEnvSet "NO_CLEANUP"
                    parseAbsDir <=< ContT
                        $ withSystemTempDir
                            (MsgTempDir >$< logTracer)
                            "test-cluster"
                            skipCleanup
        let clusterCfg =
                Config
                    { cfgStakePools = defaultPoolConfigs
                    , cfgLastHardFork = clusterEra
                    , cfgNodeLogging
                    , cfgClusterDir = clusterPath
                    , cfgClusterConfigs = clusterConfigsDir
                    , cfgTestnetMagic = TestnetMagic 42
                    , cfgShelleyGenesisMods = [over #sgSlotLength \_ -> 0.2]
                    , cfgTracer = logTracer
                    , cfgNodeOutputFile = Nothing
                    , cfgRelayNodePath = [reldir|relay|]
                    , cfgClusterLogFile = clusterLogs
                    , cfgNodeToClientSocket = nodeToClientSocket
                    }
        nodeConnectionVar <- lift mkCardanoNodeConnVar
        phaseTracer <-
            withMonitoring
                clusterControl
                ((,) <$> monitoring <*> pure testnetMonitorAPI)
                clusterCfg
                nodeConnectionVar
        lift $ traceWith phaseTracer RetrievingFunds
        funds <- lift $ retrieveFunds faucetFundsFile
        conn <- ContT $ withCluster phaseTracer clusterCfg funds
        lift
            $ writeCardanoNodeConnVar nodeConnectionVar
            $ runningNodeSocketPath conn

        lift $ threadDelay maxBound -- wait for Ctrl+C
