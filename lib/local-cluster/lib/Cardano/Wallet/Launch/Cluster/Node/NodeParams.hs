{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (..)
    , singleNodeParams
    )
where

import Prelude

import Cardano.BM.Tracing
    ( Severity
    )
import Cardano.Launcher.Node
    ( MaybeK
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (BabbageHardFork)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    )
import Cardano.Wallet.Launch.Cluster.GenesisFiles
    ( GenesisFiles
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( LogFileConfig (..)
    )

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams d = NodeParams
    { nodeGenesisFiles :: GenesisFiles
    -- ^ Genesis block start time
    , nodeHardForks :: ClusterEra
    -- ^ Era to hard fork into.
    , nodePeers :: (Int, [Int])
    -- ^ A list of ports used by peers and this node
    , nodeLogConfig :: LogFileConfig
    -- ^ The node will always log to "cardano-node.log" relative to the
    -- config. This option can set the minimum severity and add another output
    -- file.
    , nodeParamsOutputFile :: Maybe (AbsFileOf "node-output")
    , nodeSocket :: MaybeK (AbsFileOf "node-to-client-socket") d
    }
    deriving stock (Show)

singleNodeParams
    :: GenesisFiles
    -> Severity
    -> Maybe (FilePath, Severity)
    -> Maybe (AbsFileOf "node-output")
    -> MaybeK (AbsFileOf "node-to-client-socket") d
    -> NodeParams d
singleNodeParams genesisFiles severity extraLogFile =
    NodeParams genesisFiles BabbageHardFork (0, []) LogFileConfig
        { minSeverityTerminal = severity
        , extraLogDir = fmap fst extraLogFile
        , minSeverityFile = maybe severity snd extraLogFile
        }
