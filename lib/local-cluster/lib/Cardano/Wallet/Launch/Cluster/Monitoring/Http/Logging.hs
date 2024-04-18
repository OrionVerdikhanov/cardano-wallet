{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Logging
    ( MsgHttpMonitoring (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( MsgClient
    )
import Network.Socket
    ( PortNumber
    )

data MsgHttpMonitoring
    = MsgHttpMonitoringPort PortNumber
    | MsgHttpMonitoringQuery MsgClient
    deriving stock (Show)
