{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Server
    ( withHttpServer
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( Phase (..)
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
import Data.Foldable
    ( find
    )
import Network.Wai.Handler.Warp
    ( Port
    , run
    )
import Servant
    ( Application
    , Proxy (..)
    )
import Servant.Server
    ( serve
    )
import UnliftIO
    ( MonadUnliftIO
    , UnliftIO (..)
    , async
    , link
    , withUnliftIO
    )

type Phases m a = Monitor m a [Phase]

isReady :: Phase -> Bool
isReady (Cluster _) = True
isReady _ = False

handlePhase :: (Monad m) => Phases m a -> m Bool
handlePhase Monitor{..} = do
    s <- fst <$> observe
    case find isReady s of
        Just _ -> pure True
        Nothing -> pure False

server :: MonadUnliftIO m => Phases m a -> m Application
server l = withUnliftIO $ \(UnliftIO u) -> do
    let z = liftIO . u
    pure $ serve (Proxy @API) $ z $ handlePhase l

httpServer :: MonadUnliftIO m => Port -> Phases m a -> m ()
httpServer port monitor = do
    app <- server monitor
    liftIO $ run port app

-- | Run a HTTP server that serves the monitoring API
-- We use CPS even if it's not needed in this implementation as a
-- future-proofing measure.
withHttpServer :: MonadUnliftIO m => Port -> Phases m a -> (() -> m b) -> m b
withHttpServer port monitor action = do
    link <=< async $ httpServer port monitor
    action ()
