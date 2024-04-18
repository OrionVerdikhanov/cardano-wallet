{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Server
    ( withHttpServer
    , mkHandlers
    , Handlers
    )
where

import Prelude

import Cardano.Address
    ( unsafeMkAddress
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( runClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( sendFaucetAssetsTo
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( MonitorApi
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Http.SendFaucetAssets
    ( SendFaucetAssets (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Control.Monad
    ( (<=<)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monitoring.Monitor
    ( Monitor (..)
    )
import Data.Bifunctor
    ( first
    )
import Data.Foldable
    ( find
    )
import Data.Functor
    ( (<&>)
    )
import Network.Wai.Handler.Warp
    ( Port
    , run
    )
import Servant
    ( Application
    , NoContent (..)
    , err500
    , (:<|>) (..)
    )
import Servant.Server
    ( ServerError (..)
    , serve
    )
import UnliftIO
    ( async
    , link
    , throwIO
    )

--- handle ready --------------------------------------------------------------

isReady :: Phase -> Bool
isReady (Cluster _) = True
isReady _ = False

mkHandleReady :: Monitor IO a [Phase] -> IO Bool
mkHandleReady Monitor{..} = do
    s <- fst <$> observe
    case find isReady s of
        Just _ -> pure True
        Nothing -> pure False

--- handle send faucet assets -------------------------------------------------

type HandleSendFaucetAssets =
    Int
    -> [(Address, (TokenBundle, [(String, String)]))]
    -> IO ()

mkHandleSendFaucetAssets
    :: IO (Maybe CardanoNodeConn)
    -> Config
    -> HandleSendFaucetAssets
mkHandleSendFaucetAssets tconn config n movements = do
    mconn <- tconn
    case mconn of
        Nothing -> throwIO err500{errBody = "Not yet connected to the node"}
        Just conn ->
            runClusterM config
                $ sendFaucetAssetsTo conn n
                $ movements
                <&> first (unsafeMkAddress . unAddress)

-- | Create handlers for the monitoring API
mkHandlers
    :: Monitor IO a [Phase]
    -> IO (Maybe CardanoNodeConn)
    -> Config
    -> Handlers
mkHandlers phases tconn config =
    Handlers
        { handleReady = mkHandleReady phases
        , handleSendFauctetAssets = mkHandleSendFaucetAssets tconn config
        }

-- | Handlers for the monitoring API, opaque.
data Handlers = Handlers
    { handleReady :: IO Bool
    , handleSendFauctetAssets :: HandleSendFaucetAssets
    }

handleSendFauctetAssetsH
    :: Handlers
    -> SendFaucetAssets n
    -> IO NoContent
handleSendFauctetAssetsH
    Handlers{handleSendFauctetAssets}
    (SendFaucetAssets n xs) = do
        handleSendFauctetAssets n xs
        pure NoContent

server
    :: HasSNetworkId n
    => MonitorApi n
    -> Handlers
    -> IO Application
server api handlers = do
    let lIO = liftIO
    pure
        $ serve api
        $ lIO (handleReady handlers)
            :<|> (lIO . handleSendFauctetAssetsH handlers)

httpServer :: HasSNetworkId n => MonitorApi n -> Port -> Handlers -> IO ()
httpServer api port handlers = do
    app <- server api handlers
    liftIO $ run port app

-- | Run a HTTP server that serves the monitoring API
-- We use CPS even if it's not needed in this implementation as a
-- future-proofing measure.
withHttpServer
    :: HasSNetworkId n
    => MonitorApi n
    -> Port
    -> Handlers
    -> (() -> IO b)
    -> IO b
withHttpServer api port handlers action = do
    link <=< async $ httpServer api port handlers
    action ()
