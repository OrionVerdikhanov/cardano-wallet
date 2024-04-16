{-# LANGUAGE QuasiQuotes #-}
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
import Path
    ( File
    , Path
    , Rel
    , addExtension
    , fromAbsDir
    , fromAbsFile
    , fromRelFile
    , parseRelFile
    , reldir
    , (</>)
    )
import System.Directory
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
    , urlFromPoolIndex :: Int -> IO String
    }

withPoolMetadataServer
    :: (PoolMetadataServer -> ClusterM a)
    -> ClusterM a
withPoolMetadataServer action = do
    UnliftClusterM withConfig Config{..} <- askUnliftClusterM
    let metadir = cfgClusterDir </> [reldir|pool-metadata|]
        metadirFilePath = fromAbsDir metadir
    liftIO $ do
        createDirectoryIfMissing False metadirFilePath
        withStaticServer metadirFilePath $ \baseURL -> do
            let mkURL mdf = baseURL <> "/" <> fromRelFile mdf
            withConfig
                $ action
                    PoolMetadataServer
                        { registerMetadataForPoolIndex = \i metadata -> do
                            let metadataBytes = Aeson.encode metadata
                            mdf <- metadataFileName i
                            BL8.writeFile (fromAbsFile $ metadir </> mdf) metadataBytes
                            let hash = blake2b256 (BL.toStrict metadataBytes)
                            traceWith cfgTracer
                                $ MsgRegisteringPoolMetadata
                                    (mkURL mdf)
                                    (B8.unpack $ convertToBase Base16 hash)
                        , urlFromPoolIndex = \i -> do
                                    mdf <- metadataFileName i
                                    pure $ mkURL mdf
                        }
  where
    metadataFileName :: Int -> IO (Path Rel File)
    metadataFileName i = do
        parseRelFile (show i) >>= addExtension ".json"
