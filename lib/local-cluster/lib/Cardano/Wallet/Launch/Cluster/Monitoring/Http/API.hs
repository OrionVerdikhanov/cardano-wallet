{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( API
    , MonitorApi
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.SendFaucetAssets
    ( SendFaucetAssets
    )
import Data.Proxy
    ( Proxy (..)
    )
import Servant
    ( PostNoContent
    )
import Servant.API
    ( Get
    , JSON
    , ReqBody
    , (:<|>)
    , (:>)
    )

type API n =
    "ready" :> Get '[JSON] Bool
        :<|> "send" :> ReqBody '[JSON] (SendFaucetAssets n):> PostNoContent

type MonitorApi n = Proxy (API n)
