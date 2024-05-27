{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}

module Cardano.Wallet.Launch.Cluster.Http.ServiceSpec
    ( spec
    )
where

import Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , newToTextTracer
    )
import Cardano.Chain.Common
    ( unsafeGetLovelace
    )
import Cardano.Launcher
    ( Command (..)
    , IfToSendSigINT (..)
    , TimeoutInSecs (..)
    , withBackendProcess
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds (FaucetFunds)
    , FileOf (..)
    , RunningNode (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet.Serialize
    ( saveFunds
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.Client
    ( RunFaucetQ
    )
import Cardano.Wallet.Launch.Cluster.Http.Monitor.Client
    ( MonitorQ (..)
    , RunMonitorQ (..)
    )
import Cardano.Wallet.Launch.Cluster.Http.Service
    ( ServiceConfiguration (..)
    , withService
    , withServiceClient
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer (currentNodeTip)
    )
import Cardano.Wallet.Network.Implementation
    ( withNetworkLayer
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber
    , getRandomPort
    )
import Cardano.Wallet.Network.Streaming
    ( CallbackLogs
    , ChainStream
    , eraBlockS
    , eraTxS
    , forChainStream
    , forConsensusS
    , scanChainStream
    , withStreamingFromBlockChain
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromGenesisData
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    , SNetworkId (SMainnet)
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (SyncTolerance)
    )
import Cardano.Wallet.Read
    ( Era (..)
    , EraValue
    , IsEra (..)
    , K (..)
    , Tx
    , applyEraFunValue
    , extractEraValue
    , (:*:) (..)
    )
import Cardano.Wallet.Read.Tx.Outputs
    ( Outputs (..)
    , getEraOutputs
    )
import Control.Monad
    ( forM_
    , join
    , unless
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.Fix
    ( fix
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Control.Tracer
    ( Tracer
    , nullTracer
    , traceWith
    )
import Data.ByteString
    ( ByteString
    )
import Data.Foldable
    ( toList
    )
import Data.Map.Strict
    ( Map
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardCrypto
    , StandardMary
    , StandardShelley
    )
import Streaming
    ( MonadIO (liftIO)
    , Of
    , Stream
    )
import Streaming.Callbacks
    ( newTMVarBuffer
    )
import System.Environment
    ( lookupEnv
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import System.IO
    ( IOMode (..)
    , withFile
    )
import System.IO.Extra
    ( withTempFile
    )
import System.Path
    ( absFile
    )
import System.Process
    ( StdStream (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    )
import UnliftIO.Async
    ( async
    , wait
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Directory
    ( createDirectoryIfMissing
    )
import Cardano.Wallet.Network.Rollback.One
    ( oneHistory
    )

import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Alonzo.TxOut as Alonzo
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Network.Implementation as NL
import qualified Data.Map.Strict as Map
import qualified Streaming.Prelude as S

testService
    :: MonitorState
    -> (Tracer IO Phase -> RunMonitorQ IO -> IO ())
    -> IO ()
testService w f =
    evalContT $ do
        (tracer, (query, _)) <-
            withService
                SMainnet
                (error "No connection")
                (error "No cluster")
                nullTracer
                $ ServiceConfiguration Nothing w
        liftIO $ f tracer query

localClusterCommand
    :: FilePath
    -- ^ filename to append to the logs dir
    -> PortNumber
    -- ^ monitoring port
    -> FilePath
    -- ^ faucet funds path
    -> ContT r IO (Maybe FilePath, Command)
localClusterCommand name port faucetFundsPath = do
    configsPath <- liftIO getClusterConfigsPathFromEnv
    mLogsPath <- liftIO getClusterLogsFilePathFromEnv
    mMinSeverity <- liftIO getClusterLogsMinSeverity
    (clusterStdout, logsPathName) <- case mLogsPath of
        Nothing -> pure (NoStream, Nothing)
        Just logsPath -> do
            let logsPathName = logsPath </> name
            fmap (\h -> (UseHandle h, Just logsPathName))
                $ ContT
                $ withFile (logsPath </> name <> "-stdout" <.> "log") WriteMode

    pure
        $ (logsPathName,)
        $ Command
            { cmdName = "local-cluster"
            , cmdArgs =
                [ "--faucet-funds"
                , faucetFundsPath
                , "--monitoring-port"
                , show port
                , "--cluster-configs"
                , configsPath
                ]
                    <> case mLogsPath of
                        Nothing -> []
                        Just logsPath ->
                            [ "--cluster-logs"
                            , logsPath </> name <.> "log"
                            ]
                    <> case mMinSeverity of
                        Nothing -> []
                        Just minSeverity -> ["--min-severity", show minSeverity]
            , cmdSetup = pure ()
            , cmdInput = NoStream
            , cmdOutput = clusterStdout
            }

getClusterConfigsPathFromEnv :: IO FilePath
getClusterConfigsPathFromEnv = do
    lookupEnv "LOCAL_CLUSTER_CONFIGS" >>= \case
        Just path -> pure path
        Nothing -> error "LOCAL_CLUSTER_CONFIGS not set"

getClusterLogsFilePathFromEnv :: IO (Maybe FilePath)
getClusterLogsFilePathFromEnv = do
    mp <- lookupEnv "CLUSTER_LOGS_DIR_PATH"
    forM_ mp $ \dir ->
        createDirectoryIfMissing True dir
    pure mp

getClusterLogsMinSeverity :: IO (Maybe String)
getClusterLogsMinSeverity = lookupEnv "CLUSTER_LOGS_MIN_SEVERITY"

testServiceWithCluster
    :: FilePath
    -> FaucetFunds
    -> ContT () IO ((RunMonitorQ IO, RunFaucetQ IO), ToTextTracer)
testServiceWithCluster name faucetFundsValue = do
    port <- liftIO getRandomPort
    faucetFundsPath <- ContT withTempFile
    liftIO $ saveFunds (FileOf $ absFile faucetFundsPath) faucetFundsValue
    (logsPathName, command) <- localClusterCommand name port faucetFundsPath
    ToTextTracer processLogs <- case logsPathName of
        Nothing -> pure $ ToTextTracer nullTracer
        Just path ->
            ContT
                $ newToTextTracer
                    (path <> "-process" <.> "log")
                    Nothing
    _ <-
        ContT
            $ withBackendProcess
                processLogs
                command
                NoTimeout
                DoNotSendSigINT
    queries <- withSNetworkId (NTestnet 42)
        $ \network -> withServiceClient network port nullTracer
    pure (queries, ToTextTracer processLogs)

withNetwork
    :: Tracer IO NL.Log
    -> RunningNode
    -> ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto))
withNetwork tr (RunningNode sock genesisData vData) = do
    let (np, _, _) = fromGenesisData genesisData
    let sTol = SyncTolerance 60
    ContT
        $ withNetworkLayer
            tr
            tunedForMainnetPipeliningStrategy
            np
            sock
            vData
            sTol

noFunds :: FaucetFunds
noFunds = FaucetFunds [] [] []

spec :: Spec
spec = do
    describe "withService control" $ do
        it "can start" $ do
            testService Step $ \_ _ -> pure ()
        it "can query" $ do
            testService Step $ \_ (RunQuery query) -> do
                result <- query ReadyQ
                result `shouldBe` False
        it "can trace" $ do
            testService Run $ \tracer _ -> do
                traceWith tracer RetrievingFunds
        it "can report readiness" $ do
            testService Run $ \tracer (RunQuery query) -> do
                traceWith tracer (Cluster Nothing)
                result <- query ReadyQ
                result `shouldBe` True
        it "can step the tracer thread" $ do
            testService Step $ \tracer (RunQuery query) -> do
                tracer' <- async $ do
                    traceWith tracer (Cluster Nothing)
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ query StepQ >> loop
                wait tracer'
        it "can report the phase history" $ do
            testService Run $ \tracer (RunQuery query) -> do
                traceWith tracer RetrievingFunds
                traceWith tracer Metadata
                traceWith tracer Genesis
                traceWith tracer Pool0
                traceWith tracer Funding
                traceWith tracer Pools
                traceWith tracer Relay
                traceWith tracer (Cluster Nothing)
                threadDelay 10000
                (History phases, state) <- query ObserveQ
                snd <$> phases
                    `shouldBe` [ RetrievingFunds
                               , Metadata
                               , Genesis
                               , Pool0
                               , Funding
                               , Pools
                               , Relay
                               , Cluster Nothing
                               ]
                state `shouldBe` Run
        it "can switch from step to run" $ do
            testService Step $ \tracer (RunQuery query) -> do
                tracer' <- async $ do
                    traceWith tracer RetrievingFunds
                state <- query SwitchQ
                state `shouldBe` Run
                wait tracer'
                (History phases, _state) <- query ObserveQ
                snd <$> phases `shouldBe` [RetrievingFunds]
    describe "withService application" $ do
        it "can start and stop" $ evalContT $ do
            ((RunQuery query, _), _) <-
                testServiceWithCluster
                    "can-start-and-stop"
                    noFunds
            liftIO $ do
                result <- query ReadyQ
                result `shouldBe` False
        it "can wait for cluster ready before ending" $ evalContT $ do
            ((RunQuery query, _), _) <-
                testServiceWithCluster
                    "can-wait-for-cluster-ready-before-ending"
                    noFunds
            liftIO $ do
                fix $ \loop -> do
                    result <- query ReadyQ
                    unless result $ threadDelay 10000 >> loop
    describe "withNetwork" $ do
        it "can start and stop" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                testServiceWithCluster
                    "withNetwork-can-start-and-stop"
                    noFunds
            node <- liftIO $ waitForNode query
            network <- withNetwork tr node
            tip <- liftIO $ currentNodeTip network
            tip `seq` pure ()
        it "can get the first block" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                testServiceWithCluster
                    "withNetwork-can-get-collect-the-incoming-blocks"
                    noFunds
            node <- liftIO $ waitForNode query
            network <- withNetwork tr node
            blocks <- withStreamingFromBlockChain network tr newTMVarBuffer
            firstBlock <- liftIO
                    $ S.effects -- discard logs
                    $ S.head_ -- get the first element
                    $ elements blocks
            liftIO $ join firstBlock `shouldNotBe` Nothing
        it "can get the first non-empty balance" $ evalContT $ do
            ((query, _), ToTextTracer tr) <-
                testServiceWithCluster
                    "withNetwork-can-get-collect-the-incoming-blocks"
                    noFunds
            node <- liftIO $ waitForNode query
            network <- withNetwork tr node
            blocks <- withStreamingFromBlockChain network tr newTMVarBuffer
            firstBalance <- liftIO
                $ S.effects
                $ S.head_
                $ balance
                $ outputs
                $ eraTxS
                $ eraBlockS
                $ forConsensusS blocks
            liftIO $ firstBalance `shouldNotBe` Nothing

waitForNode :: RunMonitorQ IO -> IO RunningNode
waitForNode (RunQuery query) = fix $ \loop -> do
    (history', _) <- query ObserveQ
    case getNode history' of
        Nothing -> threadDelay 10000 >> loop
        Just node -> pure node

getNode :: History -> Maybe RunningNode
getNode (History phases) = case phases of
    [] -> Nothing
    (_time, phase) : _ -> case phase of
        Cluster (Just node) -> Just node
        _ -> Nothing

data TxOut = TxOut
    { address :: ByteString
    , value :: Integer
    }
    deriving stock (Show, Eq)

outputs
    :: ChainStream (EraValue (ctx :*: Tx)) ()
    -> ChainStream TxOut ()
outputs = forChainStream $ extractEraValue . applyEraFunValue f
  where
    f :: IsEra era => (ctx :*: Tx) era -> K [TxOut] era
    f (_bh :*: tx) = txOutFromOutput $ getEraOutputs tx

elements
    :: ChainStream a r
    -> Stream
        (Of (Maybe a))
        (Stream (Of (CallbackLogs a)) IO)
        r
elements = scanChainStream (const Just) $ oneHistory 10 Nothing

balance
    :: ChainStream TxOut r
    -> Stream
        (Of (Map ByteString Integer))
        ( Stream (Of (CallbackLogs TxOut)) IO
        )
        r
balance =
    scanChainStream
        (\m (TxOut addr val) -> Map.insertWith (+) addr val m)
        $ oneHistory 10 Map.empty

txOutFromOutput :: forall era. IsEra era => Outputs era -> K [TxOut] era
txOutFromOutput = case theEra @era of
    Byron -> \(Outputs os) -> K $ fromByronTxOut <$> toList os
    Shelley -> \(Outputs os) -> K $ fromShelleyTxOut <$> toList os
    Allegra -> \(Outputs os) -> K $ fromAllegraTxOut <$> toList os
    Mary -> \(Outputs os) -> K $ fromMaryTxOut <$> toList os
    Alonzo -> \(Outputs os) -> K $ fromAlonzoTxOut <$> toList os
    Babbage -> \(Outputs os) -> K $ fromBabbageTxOut <$> toList os
    Conway -> \(Outputs os) -> K $ fromConwayTxOut <$> toList os
  where
    fromByronTxOut :: Byron.TxOut -> TxOut
    fromByronTxOut (Byron.TxOut addr amount) =
        TxOut (serialize' addr) (fromIntegral $ unsafeGetLovelace amount)

    fromShelleyTxOut :: SL.ShelleyTxOut StandardShelley -> TxOut
    fromShelleyTxOut (SL.ShelleyTxOut addr (Coin amount)) =
        TxOut (SL.serialiseAddr addr) amount

    fromAllegraTxOut :: SL.ShelleyTxOut StandardAllegra -> TxOut
    fromAllegraTxOut (SL.ShelleyTxOut addr (Coin amount)) =
        TxOut (SL.serialiseAddr addr) amount

    fromMaryTxOut :: SL.ShelleyTxOut StandardMary -> TxOut
    fromMaryTxOut (SL.ShelleyTxOut addr (MaryValue (Coin amount) _)) =
        TxOut (SL.serialiseAddr addr) amount

    fromAlonzoTxOut :: Alonzo.AlonzoTxOut StandardAlonzo -> TxOut
    fromAlonzoTxOut (Alonzo.AlonzoTxOut addr (MaryValue (Coin amount) _) _) =
        TxOut (SL.serialiseAddr addr) amount

    fromBabbageTxOut :: Babbage.BabbageTxOut StandardBabbage -> TxOut
    fromBabbageTxOut
        (Babbage.BabbageTxOut addr (MaryValue (Coin amount) _) _ _) =
            TxOut (SL.serialiseAddr addr) amount

    fromConwayTxOut :: Babbage.BabbageTxOut StandardConway -> TxOut
    fromConwayTxOut
        (Babbage.BabbageTxOut addr (MaryValue (Coin amount) _) _ _) =
            TxOut (SL.serialiseAddr addr) amount
