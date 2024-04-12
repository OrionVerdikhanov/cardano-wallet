{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Monitor
    ( withMonitoring
    , withHttpMonitoring
    , MsgHttpMonitoring (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CommandLine
    ( ClusterControl (..)
    , Monitoring (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( MsgClient
    , RunQuery
    , withHttpClient
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Server
    ( withHttpServer
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber
    , getRandomPort
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans
    ( MonadTrans (..)
    )
import Control.Monad.Trans.Resource
    ( MonadUnliftIO
    )
import Control.Monitoring
    ( Monitor
    , MonitorState (..)
    , mkFoldingMonitor
    , mkMonitor
    , runTCPControl
    , trace
    )
import Control.Tracer
    ( Tracer (..)
    , nullTracer
    , traceWith
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Map
    ( Map
    )
import Data.Profunctor
    ( Profunctor (..)
    )
import Data.Time
    ( UTCTime
    , getCurrentTime
    )

import qualified Control.Foldl as F

withClusterControl
    :: (MonadUnliftIO m, Show a)
    => Int
    -- ^ Monitoring port
    -> Monitor m a (Map UTCTime a)
    -- ^ Initial monitor state
    -> (() -> m r)
    -- ^ Action to run with the monitor tracer
    -> m r
withClusterControl monitoringPort =
    runTCPControl monitoringPort (fmap show . toList)

timedMonitor :: MonadIO m => MonitorState -> m (Monitor m a (Map UTCTime a))
timedMonitor initialState = do
    t <- mkFoldingMonitor (liftIO getCurrentTime) F.map initialState
    mkMonitor t

-- | Start monitoring services as required by the given command line options
-- The `Monitor` will be shared between the TCP and HTTP servers in case both
-- are enabled.
-- This implies the offered phase tracer will be the same for both.
withMonitoring
    :: MonadUnliftIO m
    => Maybe ClusterControl
    -- ^ tcp cluster control options
    -> Maybe Monitoring
    -- ^ http monitoring options
    -> ContT r m (Tracer m Phase)
withMonitoring mClusterControl mMonitoring = do
    let tcp ClusterControl{..} = do
            monitor <- lift $ timedMonitor pullingMode
            ContT $ withClusterControl clusterControlPort monitor
            pure monitor
        http cMonitor Monitoring{..} = do
            monitor <- maybe (lift $ timedMonitor NotPullingState) pure cMonitor
            ContT $ withHttpServer monitoringPort $ rmap toList monitor
            pure monitor
        maybeM m f = maybe (pure Nothing) (fmap Just <$> f) m
    cMonitor <- maybeM mClusterControl tcp
    mMonitor <- maybeM mMonitoring $ http cMonitor
    pure $ maybe nullTracer (Tracer . trace) mMonitor

data MsgHttpMonitoring
    = MsgHttpMonitoringPort PortNumber
    | MsgHttpMonitoringQuery MsgClient
    deriving stock (Show)

-- | This will create a client for a monitoring system and a monitor command line
-- option that will start a monitoring server on the same port the client is listening to.
withHttpMonitoring
    :: MonadUnliftIO m
    => Tracer m MsgHttpMonitoring
    -> ContT b m (Monitoring, RunQuery m)
withHttpMonitoring tr =
    do
        httpPort <- liftIO getRandomPort
        lift $ traceWith tr $ MsgHttpMonitoringPort httpPort
        monitorClient <-
            withHttpClient (MsgHttpMonitoringQuery >$< tr)
                $ fromIntegral httpPort
        pure (Monitoring $ fromIntegral httpPort, monitorClient)
