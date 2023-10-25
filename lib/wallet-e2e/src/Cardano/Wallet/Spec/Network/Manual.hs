module Cardano.Wallet.Spec.Network.Manual where

import Cardano.Wallet.Cli.Launcher
    ( WalletApi (..)
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork (..)
    )

configuredNetwork :: ConfiguredNetwork
configuredNetwork =
    ConfiguredNetwork
        { configuredNetworkWallet =
            WalletApi
                { walletInstanceApiUrl = "http://localhost:8090/v2"
                , walletInstanceApiHost = "localhost"
                , walletInstanceApiPort = 8090
                }
        }
