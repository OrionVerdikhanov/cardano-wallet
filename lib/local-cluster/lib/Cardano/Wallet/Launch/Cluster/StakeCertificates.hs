{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Launch.Cluster.StakeCertificates
    ( issueStakeVkCert
    , issueStakeScriptCert
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    , clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( AbsFileOf
    )
import Control.Monad.Reader
    ( asks
    )
import Data.Generics.Labels
    ()
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import Path
    ( fromAbsFile
    , parseRelFile
    , (</>)
    )

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tagged "prefix" String
    -> AbsFileOf "stake-pub"
    -> ClusterM (AbsFileOf "stake-vk-cert")
issueStakeVkCert prefix stakePub = do
    outputDir <- asks cfgClusterDir
    lastHardFork <- asks cfgLastHardFork
    stakeCertFile <- parseRelFile $ untag prefix <> "-stake.cert"
    let file = outputDir </> stakeCertFile
    cli $
        [ clusterEraToString lastHardFork
        , "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , fromAbsFile stakePub
        , "--out-file"
        , fromAbsFile file
        ] <> case lastHardFork of
            BabbageHardFork -> []
            ConwayHardFork -> [
                "--key-reg-deposit-amt"
                , "1000000"
                ]
    pure file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tagged "prefix" String
    -> FilePath
    -> ClusterM (AbsFileOf "stake-script-cert")
issueStakeScriptCert prefix stakeScript = do
    outputDir <- asks cfgClusterDir
    stakeCertFile <- parseRelFile $ untag prefix <> "-stake.cert"
    let file = outputDir </> stakeCertFile
    cli
        [ "stake-address"
        , "registration-certificate"
        , "--stake-script-file"
        , stakeScript
        , "--out-file"
        , fromAbsFile file
        ]
    pure file
