{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    , proxyAPI
    )
where

import Data.Proxy
    ( Proxy (..)
    )
import Prelude
import Servant.API
    ( Get
    , JSON
    , (:>)
    )

type API = "ready" :> Get '[JSON] Bool

proxyAPI :: Proxy API
proxyAPI = Proxy
