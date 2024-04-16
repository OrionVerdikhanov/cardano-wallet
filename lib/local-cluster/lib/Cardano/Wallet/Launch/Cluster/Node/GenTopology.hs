{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , askNodeDir
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    , RelDirOf
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Data.Aeson
    ( (.=)
    )
import Path
    ( fromAbsFile
    , relfile
    , (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a topology file from a list of peers.
genTopology
    :: RelDirOf "node"
    -> [Int]
    -> ClusterM (AbsFileOf "topology")
genTopology nodeSegment peers = do
    nodeDir <- askNodeDir nodeSegment
    let file = nodeDir </> [relfile|node.topology|]
    liftIO
        $ Aeson.encodeFile (fromAbsFile file)
        $ Aeson.object ["Producers" .= map encodePeer peers]
    pure file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]
