{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Wallet.Launch.Cluster.Node.Relay
    ( withRelayNode
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , MaybeK (..)
    , NodePort (..)
    , Presence (..)
    , fmapMaybeK
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , askNodeDir
    , bracketTracer'
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( RelDirOf
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( NodeId (..)
    , setLoggingName
    )
import Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
import Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
import Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (..)
    )
import Cardano.Wallet.Launch.Cluster.Node.Process
    ( withCardanoNodeProcess
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (..)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Data.Tagged
    ( Tagged (..)
    )
import Path
    ( fromAbsDir
    , fromAbsFile
    , reldir
    , toFilePath
    , (</>)
    )
import System.Directory
    ( createDirectoryIfMissing
    )

-- | Launches a @cardano-node@ with the given configuration which will not forge
-- blocks, but has every other cluster node as its peer. Any transactions
-- submitted to this node will be broadcast to every node in the cluster.
--
-- Connecting wallet to a block-producing (pool) node could cause problems
-- with the block production: wallet sends resource-heavy queries and that
-- causes timeout and breaks connection with other nodes;
--
-- Connectiong wallet to a non-block producing (relay) node allows to avoid
-- such problems.
withRelayNode
    :: NodeParams Present
    -- ^ Parameters used to generate config files.
    -> RelDirOf "node"
    -- ^ Path segment for the node to add to the cluster directory.
    -> (RunningNode -> ClusterM a)
    -- ^ Callback function with socket path
    -> ClusterM a
withRelayNode params nodeSegment onClusterStart = do
    let name = toFilePath nodeSegment
    relayDir <- askNodeDir nodeSegment
    let NodeParams genesisFiles hardForks (port, peers) logCfg _ msocket = params
    bracketTracer' "withRelayNode" $ do
        liftIO $ createDirectoryIfMissing True $ fromAbsDir relayDir
        let logCfg' = setLoggingName name logCfg
        (nodeConfig, genesisData, vd) <-
            genNodeConfig
                nodeSegment
                (Tagged @"node-name" "-relay")
                genesisFiles
                hardForks
                logCfg'
        topology <- genTopology nodeSegment peers
        let cfg =
                CardanoNodeConfig
                    { nodeDir = fromAbsDir relayDir
                    , nodeConfigFile = fromAbsFile nodeConfig
                    , nodeTopologyFile = fromAbsFile topology
                    , nodeDatabaseDir = fromAbsDir $ relayDir </> [reldir|db|]
                    , nodeDlgCertFile = Nothing
                    , nodeSignKeyFile = Nothing
                    , nodeOpCertFile = Nothing
                    , nodeKesKeyFile = Nothing
                    , nodeVrfKeyFile = Nothing
                    , nodePort = Just (NodePort port)
                    , nodeLoggingHostname = Just name
                    , nodeExecutable = Nothing
                    , nodeOutputFile = fromAbsFile <$> nodeParamsOutputFile params
                    , nodeSocketPathFile = fmapMaybeK fromAbsFile msocket
                    }
        withCardanoNodeProcess RelayNode cfg $ \(JustK socket) ->
            onClusterStart
                $ RunningNode socket genesisData vd
