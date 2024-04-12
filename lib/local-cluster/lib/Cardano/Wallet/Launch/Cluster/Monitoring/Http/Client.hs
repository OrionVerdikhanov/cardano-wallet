{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.Client
    ( withHttpClient
    , RunQuery (..)
    , Query (..)
    , MsgClient (..)
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    )
import Control.Monad
    ( unless
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Retry
    ( RetryPolicyM
    , RetryStatus (..)
    , capDelay
    , exponentialBackoff
    , recoverAll
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Proxy
    ( Proxy (..)
    )
import Network.HTTP.Client
    ( ManagerSettings (..)
    , defaultManagerSettings
    , newManager
    , responseTimeoutNone
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , runClientM
    )
import UnliftIO
    ( MonadUnliftIO
    , UnliftIO (..)
    , askUnliftIO
    , throwIO
    )

-- | Queries that can be sent to the monitoring server via HTTP.
data Query a where
    Ready :: Query Bool

ready :: ClientM Bool
ready = client (Proxy @API)

data AnyQuery = forall a. Show a => AnyQuery (Query a)

instance Show AnyQuery where
    show (AnyQuery Ready) = "Ready"

-- | Run any query against the monitoring server.
newtype RunQuery m = RunQuery (forall a. Show a => Query a -> m a)

data MsgClient
    = MsgClientReq AnyQuery
    | MsgClientRetry AnyQuery
    deriving stock (Show)

-- | Produce a closure over the http client of an http monitoring server that
-- can be used to query the server.
withHttpClient
    :: MonadUnliftIO m
    => Tracer m MsgClient
    -> Int
    -- ^ Monitoring port to attach to (http://localhost is hardcoded)
    -> ContT b m (RunQuery m)
withHttpClient tracer httpPort = ContT $ \k -> do
    UnliftIO unlift <- askUnliftIO
    let url = BaseUrl Http "localhost" httpPort ""
        tr = traceWith tracer
    manager <-
        liftIO
            $ newManager
            $ defaultManagerSettings
                { managerResponseTimeout = responseTimeoutNone
                }
    k $ RunQuery $ \x -> do
        tr $ MsgClientReq $ AnyQuery x
        case x of
            Ready -> liftIO
                $ recoverAll retryPolicy
                $ \rt -> do
                    unless (firstTry rt)
                        $ unlift
                        $ tr
                        $ MsgClientRetry
                        $ AnyQuery x
                    r <- runClientM ready $ mkClientEnv manager url
                    either throwIO pure r

retryPolicy :: RetryPolicyM IO
retryPolicy = capDelay (60 * oneSecond) $ exponentialBackoff oneSecond
  where
    oneSecond = 1_000_000 :: Int

firstTry :: RetryStatus -> Bool
firstTry (RetryStatus 0 _ _) = True
firstTry _ = False
