{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Cluster
    ( withCluster
    , FaucetFunds (..)
    ) where

import Prelude

import Cardano.Address
    ( Address (..)
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.Launcher
    ( ProcessHasExited (..)
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , bracketTracer'
    , runClusterM
    , traceClusterLog
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.ConfiguredPool
    ( ConfiguredPool (..)
    , configurePools
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( readFaucetAddresses
    , resetGlobals
    , sendFaucetAssetsTo
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    , changeFileOf
    )
import Cardano.Wallet.Launch.Cluster.GenesisFiles
    ( GenesisFiles (..)
    , generateGenesis
    )
import Cardano.Wallet.Launch.Cluster.KeyRegistration
    ( prepareStakeKeyRegistration
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , LogFileConfig (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
    , RelayNode (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.Relay
    ( withRelayNode
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Cardano.Wallet.Launch.Cluster.PoolMetadataServer
    ( withPoolMetadataServer
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( forM
    , forM_
    , replicateM
    , replicateM_
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadTrans (..)
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Either
    ( isLeft
    , isRight
    )
import Data.Generics.Labels
    ()
import Data.List
    ( nub
    , permutations
    , sort
    )
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.Exit
    ( ExitCode (..)
    )
import UnliftIO.Async
    ( async
    , link
    , wait
    )
import UnliftIO.Chan
    ( newChan
    , readChan
    , writeChan
    )
import UnliftIO.Exception
    ( SomeException
    , finally
    , handle
    , throwIO
    )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data FaucetFunds = FaucetFunds
    { pureAdaFunds :: [(Address, Coin)]
    , maryAllegraFunds :: [(Address, (TokenBundle, [(String, String)]))]
    -- ^ Multi asset funds. Slower to setup than pure ada funds.
    --
    -- Beside the assets, there is a list of
    -- @(signing key, verification key hash)@, so that they can be minted by
    -- the faucet.
    , massiveWalletFunds :: [(Address, Coin)]
    }
    deriving stock (Eq, Show)

-- | Execute an action after starting a cluster of stake pools. The cluster also
-- contains a single BFT node that is pre-configured with keys available in the
-- test data.
--
-- This BFT node is essential in order to bootstrap the chain and allow
-- registering pools. Passing `0` as a number of pool will simply start a single
-- BFT node.
--
-- The cluster is configured to automatically hard fork to Shelley at epoch 1
-- and then to Allegra at epoch 2. Callback actions can be provided to run
-- a little time after the hard forks are scheduled.
--
-- The onClusterStart actions are not guaranteed to use the same node.
withCluster
    :: HasCallStack
    => Tracer IO Phase
    -> Config
    -> FaucetFunds
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withCluster phaseChange config@Config{..} faucetFunds onClusterStart =
    runClusterM config
        $ bracketTracer' "withCluster"
        $ flip runContT pure
        $ do
            let clusterDir = pathOf cfgClusterDir
            lift $ traceClusterLog $ MsgHardFork cfgLastHardFork
            phase Metadata
            metadataServer <- ContT withPoolMetadataServer
            liftIO $ createDirectoryIfMissing True clusterDir
            lift $ traceClusterLog $ MsgStartingCluster clusterDir
            liftIO resetGlobals

            configuredPools <- lift $ configurePools metadataServer cfgStakePools
            phase Genesis
            addGenesisPools <- lift $ do
                genesisDeltas <- mapM registerViaShelleyGenesis configuredPools
                pure $ foldr (.) id genesisDeltas
            -- TODO (yura): Use Faucet API isntead of these fixed addresses
            faucetAddresses <-
                lift
                    $ map (,Coin 1_000_000_000_000_000)
                        <$> readFaucetAddresses

            genesisFiles <-
                lift
                    $ generateGenesis
                        (pureAdaFunds <> faucetAddresses <> massiveWalletFunds)
                        (addGenesisPools : cfgShelleyGenesisMods)
            phase Genesis
            extraPort : poolsTcpPorts <-
                liftIO
                    $ randomUnusedTCPPorts (length cfgStakePools + 1)

            let pool0port :| poolPorts = NE.fromList (rotate poolsTcpPorts)
            let pool0 :| otherPools = configuredPools

            let pool0Cfg =
                    NodeParams
                        genesisFiles
                        cfgLastHardFork
                        pool0port
                        cfgNodeLogging
                        nodeOutputFile
            phase Pool0
            runningPool0 <- ContT $ operatePool pool0 pool0Cfg
            phase Funding
            lift $ extraClusterSetupUsingNode configuredPools runningPool0
            case NE.nonEmpty otherPools of
                Nothing -> do
                    phase $ Cluster Nothing
                    liftIO $ onClusterStart runningPool0
                Just others -> do
                    let relayNodeParams =
                            NodeParams
                                { nodeGenesisFiles = genesisFiles
                                , nodeHardForks = cfgLastHardFork
                                , nodePeers = (extraPort, poolsTcpPorts)
                                , nodeLogConfig =
                                    LogFileConfig
                                        { minSeverityTerminal = Info
                                        , extraLogDir = Nothing
                                        , minSeverityFile = Info
                                        }
                                , nodeParamsOutputFile = nodeOutputFile
                                }
                    phase Pools
                    _ <-
                        ContT
                            $ launchPools others genesisFiles poolPorts runningPool0
                    phase Relay
                    c <- ContT $ withRelayNode relayNodeParams cfgRelayNodePath
                    phase $ Cluster $ Just $ RelayNode $ runningNodeSocketPath c
                    liftIO $ onClusterStart c
  where
    phase = liftIO . traceWith phaseChange
    FaucetFunds pureAdaFunds maryAllegraFunds massiveWalletFunds =
        faucetFunds
    nodeOutputFile = pathOf <$> cfgNodeOutputFile
    -- Important cluster setup to run without rollbacks
    extraClusterSetupUsingNode
        :: NonEmpty ConfiguredPool -> RunningNode -> ClusterM ()
    extraClusterSetupUsingNode configuredPools runningNode = do
        let RunningNode conn _ _ = runningNode
        -- Submit retirement certs for all pools using the connection to
        -- the only running first pool to avoid the certs being rolled
        -- back.
        --
        -- We run these second in hope that it reduces the risk that any of the
        -- txs fail to make it on-chain. If this were to happen when running the
        -- integration tests, the integration tests /will fail/ (c.f. #3440).
        -- Later setup is less sensitive. Using a wallet with retrying
        -- submission pool might also be an idea for the future.
        forM_ configuredPools
            $ \pool -> finalizeShelleyGenesisSetup pool runningNode

        sendFaucetAssetsTo conn 20 maryAllegraFunds

        -- Should ideally not be hard-coded in 'withCluster'
        (rawTx, faucetPrv, stakePrv) <- prepareStakeKeyRegistration
        signAndSubmitTx
            conn
            (changeFileOf @"reg-tx" @"tx-body" rawTx)
            [ changeFileOf @"faucet-prv" @"signing-key" faucetPrv
            , changeFileOf @"stake-prv" @"signing-key" stakePrv
            ]
            "pre-registered stake key"

        -- Give the above txs a chance of getting included into the chain
        -- without competition
        liftIO $ threadDelay 10_000_000

    -- \| Actually spin up the pools.
    launchPools
        :: HasCallStack
        => NonEmpty ConfiguredPool
        -> GenesisFiles
        -> [(Int, [Int])]
        -- @(port, peers)@ pairs availible for the nodes. Can be used to e.g.
        -- add a BFT node as extra peer for all pools.
        -> RunningNode
        -- \^ Backup node to run the action with in case passed no pools.
        -> (RunningNode -> ClusterM a)
        -- \^ Action to run once when the stake pools are setup.
        -> ClusterM a
    launchPools
        configuredPools
        genesisFiles
        ports
        fallbackNode
        action = do
            waitGroup <- newChan
            doneGroup <- newChan

            let poolCount = length configuredPools

            let waitAll = do
                    traceClusterLog
                        $ MsgDebug "waiting for stake pools to register"
                    replicateM poolCount (readChan waitGroup)

            let onException :: SomeException -> ClusterM ()
                onException e = do
                    traceClusterLog
                        $ MsgDebug
                        $ "exception while starting pool: "
                            <> T.pack (show e)
                    writeChan waitGroup (Left e)

            let mkConfig (port, peers) =
                    NodeParams
                        genesisFiles
                        cfgLastHardFork
                        (port, peers)
                        cfgNodeLogging
                        nodeOutputFile
            asyncs <- forM (zip (NE.toList configuredPools) ports)
                $ \(configuredPool, (port, peers)) -> do
                    async $ handle onException $ do
                        let cfg = mkConfig (port, peers)
                        operatePool configuredPool cfg
                            $ \runningPool -> do
                                writeChan waitGroup $ Right runningPool
                                readChan doneGroup
            mapM_ link asyncs
            let cancelAll = do
                    traceWith cfgTracer $ MsgDebug "stopping all stake pools"
                    replicateM_ poolCount (writeChan doneGroup ())
                    mapM_ wait asyncs

            traceClusterLog $ MsgRegisteringStakePools poolCount
            group <- waitAll
            if length (filter isRight group) /= poolCount
                then do
                    liftIO cancelAll
                    let errors = show (filter isLeft group)
                    throwIO
                        $ ProcessHasExited
                            ("cluster didn't start correctly: " <> errors)
                            (ExitFailure 1)
                else do
                    -- Run the action using the connection to the first pool,
                    -- or the fallback.
                    let node = case group of
                            [] -> fallbackNode
                            Right firstPool : _ -> firstPool
                            Left e : _ -> error $ show e
                    action node `finally` liftIO cancelAll

    -- \| Get permutations of the size (n-1) for a list of n elements, alongside
    -- with the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: HasCallStack => Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap f . permutations
      where
        f = \case
            [] -> error "rotate: impossible"
            x : xs -> (x, sort xs)
