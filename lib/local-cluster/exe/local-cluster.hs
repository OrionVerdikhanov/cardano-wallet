{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude

import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.BM.Extra
    ( stdoutTextTracer
    )
import Cardano.Launcher.Node
    ( nodeSocketFile
    )
import Cardano.Startup
    ( installSignalHandlers
    , setDefaultFilePermissions
    )
import Cardano.Wallet.Faucet
    ( runFaucetM
    )
import Cardano.Wallet.Launch.Cluster
    ( Config (..)
    , FaucetFunds (..)
    , withFaucet
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , parseCommandLineOptions
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , mkRelDirOf
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Control.Exception
    ( bracket
    )
import Control.Lens
    ( over
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans
    ( lift
    )
import Main.Utf8
    ( withUtf8
    )
import System.Environment.Extended
    ( isEnvSet
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import System.Path
    ( absDir
    , parse
    , relDir
    , relFile
    , (</>)
    )
import System.Path.Directory
    ( createDirectoryIfMissing
    )
import UnliftIO.Concurrent
    ( threadDelay
    )

import qualified Cardano.Node.Cli.Launcher as NC
import qualified Cardano.Wallet.Cli.Launcher as WC
import qualified Cardano.Wallet.Faucet as Faucet
import qualified Cardano.Wallet.Launch.Cluster as Cluster

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
main :: IO ()
main = withUtf8 $ do
    -- Handle SIGTERM properly
    installSignalHandlers (putStrLn "Terminated")

    -- Ensure key files have correct permissions for cardano-cli
    setDefaultFilePermissions

    skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
    let tr = stdoutTextTracer
    clusterEra <- Cluster.clusterEraFromEnv
    cfgNodeLogging <-
        Cluster.logFileConfigFromEnv
            $ Just
            $ mkRelDirOf
            $ Cluster.clusterEraToString clusterEra
    CommandLineOptions{clusterConfigsDir} <- parseCommandLineOptions
    flip runContT pure $ do
        clusterPath <- ContT $ withSystemTempDir tr "test-cluster" skipCleanup
        let clusterCfg =
                Cluster.Config
                    { cfgStakePools = Cluster.defaultPoolConfigs
                    , cfgLastHardFork = clusterEra
                    , cfgNodeLogging
                    , cfgClusterDir = DirOf (absDir clusterPath)
                    , cfgClusterConfigs = clusterConfigsDir
                    , cfgTestnetMagic = Cluster.TestnetMagic 42
                    , cfgShelleyGenesisMods = [over #sgSlotLength \_ -> 0.2]
                    , cfgTracer = stdoutTextTracer
                    , cfgNodeOutputFile = Nothing
                    }
        faucetClientEnv <- ContT withFaucet
        maryAllegraFunds <-
            liftIO
                $ runFaucetM faucetClientEnv
                $ Faucet.maryAllegraFunds (Coin 10_000_000) shelleyTestnet
        node <-
            ContT
                $ Cluster.withCluster
                    clusterCfg
                    Cluster.FaucetFunds
                        { pureAdaFunds = []
                        , maryAllegraFunds
                        , massiveWalletFunds = []
                        }
        let clusterDir = absDir clusterPath
            walletDir = clusterDir </> relDir "wallet"
        lift $ createDirectoryIfMissing True walletDir
        nodeSocket <-
            case parse . nodeSocketFile $ Cluster.runningNodeSocketPath node of
                Left e -> error e
                Right p -> pure p

        let walletProcessConfig =
                WC.WalletProcessConfig
                    { WC.walletDir = DirOf walletDir
                    , WC.walletNodeApi = NC.NodeApi nodeSocket
                    , WC.walletDatabase = DirOf $ clusterDir </> relDir "db"
                    , WC.walletListenHost = Nothing
                    , WC.walletListenPort = Nothing
                    , WC.walletByronGenesisForTestnet =
                        Just
                            $ FileOf
                            $ clusterDir
                                </> relFile "byron-genesis.json"
                    }
        lift
            $ bracket
                (WC.start walletProcessConfig)
                (WC.stop . fst)
            $ const
            $ threadDelay maxBound -- wait for Ctrl+C