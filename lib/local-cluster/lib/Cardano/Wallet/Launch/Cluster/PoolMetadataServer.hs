{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.PoolMetadataServer
    ( PoolMetadataServer (..)
    , withPoolMetadataServer
    )
where

import Prelude

import Cardano.BM.Tracing
    ( traceWith
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , UnliftClusterM (UnliftClusterM)
    , askUnliftClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , toFilePath
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (MsgRegisteringPoolMetadata)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import System.Path
    ( RelFile
    , relDir
    , relFile
    , (<.>)
    , (</>)
    )
import System.Path.Directory
    ( createDirectoryIfMissing
    )
import Test.Utils.StaticServer
    ( withStaticServer
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

data PoolMetadataServer = PoolMetadataServer
    { registerMetadataForPoolIndex :: Int -> Aeson.Value -> IO ()
    , urlFromPoolIndex :: Int -> String
    }

withPoolMetadataServer
    :: (PoolMetadataServer -> ClusterM a)
    -> ClusterM a
withPoolMetadataServer action = do
    UnliftClusterM withConfig Config{..} <- askUnliftClusterM
    let metadir = absDirOf cfgClusterDir </> relDir "pool-metadata"
    liftIO $ do
        createDirectoryIfMissing True metadir
        withStaticServer (toFilePath metadir) $ \baseURL -> do
            let _urlFromPoolIndex i =
                    baseURL <> toFilePath (metadataFileName i)
            withConfig
                $ action
                    PoolMetadataServer
                        { registerMetadataForPoolIndex = \i metadata -> do
                            let metadataBytes = Aeson.encode metadata
                            BL8.writeFile
                                (toFilePath $ metadir </> (metadataFileName i))
                                metadataBytes
                            let hash = blake2b256 (BL.toStrict metadataBytes)
                            traceWith cfgTracer
                                $ MsgRegisteringPoolMetadata
                                    (_urlFromPoolIndex i)
                                    (B8.unpack $ convertToBase Base16 hash)
                        , urlFromPoolIndex = _urlFromPoolIndex
                        }
  where
    metadataFileName :: Int -> RelFile
    metadataFileName i = relFile (show i) <.> "json"
