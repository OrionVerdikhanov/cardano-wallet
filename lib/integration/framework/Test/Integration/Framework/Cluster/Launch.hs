{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , ClusterLog (..)
    , FaucetFunds
    , RunningNode
    , clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , Monitoring (..)
    , renderControl
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( Query (..)
    , RunQuery (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Monitor
    ( withHttpMonitoring
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Monad
    ( void
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Trans
    ( MonadIO (..)
    , lift
    )
import Control.Retry
    ( RetryPolicyM
    , capDelay
    , exponentialBackoff
    , retrying
    )
import Control.Tracer
    ( Tracer
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
import Path
    ( fromAbsDir
    , fromAbsFile
    , parent
    , parseAbsFile
    , relfile
    , (</>)
    )
import Path.IO
    ( createDirIfMissing
    )
import System.Environment
    ( getEnvironment
    )
import System.IO.Extra
    ( IOMode (..)
    , withTempFile
    )
import System.Process.Extra
    ( CreateProcess (..)
    , StdStream (..)
    , proc
    )
import UnliftIO
    ( Handle
    , SomeException
    , finally
    , hClose
    , openFile
    , try
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS

-- | The withFile from base intercept exceptions and rethrows them after messing
-- up the exception informations. :(
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode f = do
    h <- openFile fp mode
    f h `finally` hClose h

withLocalClusterProcess
    :: HasCallStack
    => CommandLineOptions
    -> Tracer IO ClusterLog
    -> ClusterEra
    -> ContT r IO ()
withLocalClusterProcess CommandLineOptions{..} cfgTracer era = do
    myEnv <- lift getEnvironment

    let envs =
            [ ("LOCAL_CLUSTER_ERA", clusterEraToString era)
            ]
    output <- case clusterLogs of
        Nothing -> pure Inherit
        Just logFile -> do
            createDirIfMissing True (parent logFile)
            fmap UseHandle
                $ ContT
                $ withFile (fromAbsFile logFile) WriteMode

    void
        $ ContT
        $ withBackendCreateProcess
            (MsgLauncher "local-cluster" >$< cfgTracer)
        $ (proc "local-cluster" args)
            { env = Just $ myEnv ++ envs
            , std_out = output
            , std_err = output
            }
  where
    args =
        renderControl clusterControl
            <> [ "--cluster-configs"
               , fromAbsDir clusterConfigsDir
               , "--faucet-funds"
               , fromAbsFile faucetFundsFile
               , "--socket-path"
                , fromAbsFile nodeToClientSocket
               ]
            <> case clusterDir of
                Nothing -> []
                Just clusterDir' ->
                    [ "--cluster"
                    , fromAbsDir clusterDir'
                    ]
            <> case monitoring of
                Nothing -> []
                Just (Monitoring port) ->
                    [ "--monitoring-port"
                    , show port
                    ]

withFaucetFunds
    :: HasCallStack
    => FaucetFunds
    -> ContT r IO (AbsFileOf s)
withFaucetFunds faucetFunds = ContT $ \action ->
    withTempFile $ \faucetFile -> do
        faucetFilePath <- parseAbsFile faucetFile
        saveFunds faucetFilePath faucetFunds
        action faucetFilePath

withSocketPath
    :: HasCallStack
    => AbsFileOf s
    -> ContT r m CardanoNodeConn
withSocketPath socketFilePath = ContT $ \f ->
    case cardanoNodeConn $ nodeSocketPath $ fromAbsFile socketFilePath of
        Left err -> error $ "Failed to get socket path: " ++ err
        Right socketPath -> f socketPath

withGenesisData :: FromJSON a => AbsFileOf "genesis-data" -> ContT r IO a
withGenesisData shelleyGenesis = ContT $ \f -> do
    genesisContent <- BS.readFile (fromAbsFile shelleyGenesis)

    eGenesisData <- try $ Aeson.throwDecodeStrict genesisContent
    case eGenesisData of
        Left (e :: SomeException) ->
            error $ "Failed to decode genesis data: " ++ show e
        Right genesisData -> f genesisData

withLocalClusterReady :: (Query Bool -> IO Bool) -> IO ()
withLocalClusterReady queryMonitor = do
    void $ liftIO $ retrying policy (const $ pure . not) $ \_ -> do
                queryMonitor ReadyQ
    where
        policy :: RetryPolicyM IO
        policy = capDelay (120 * oneSecond) $ exponentialBackoff oneSecond
        oneSecond = 1_000_000 :: Int

-- | Run an action against a node socket,  backed by a local cluster process
withLocalCluster
    :: HasCallStack
    => Config
    -- ^ Configuration for the cluster.
    -> FaucetFunds
    -- ^ Initial faucet funds.
    -> (RunQuery IO -> RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withLocalCluster
    Config{..}
    faucetFunds
    action = do
        let
            clusterConfigsDir = cfgClusterConfigs
            shelleyGenesis = cfgClusterDir </> [relfile|shelley-genesis.json|]
            clusterDir = Just cfgClusterDir
            clusterLogs = cfgClusterLogFile
            clusterControl = Nothing
            nodeToClientSocket = cfgNodeToClientSocket
        evalContT $ do
            (monitoring, runQuery@(RunQuery queryMonitor)) <-
                withHttpMonitoring $ MsgHttpMonitoring >$< cfgTracer
            faucetFundsFile <- withFaucetFunds faucetFunds
            socketPath <- withSocketPath nodeToClientSocket
            withLocalClusterProcess
                CommandLineOptions{monitoring = Just monitoring, ..}
                cfgTracer
                cfgLastHardFork
            liftIO $ withLocalClusterReady queryMonitor
            genesisData <- withGenesisData shelleyGenesis
            lift
                $ action runQuery
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
