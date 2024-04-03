{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    ) where

import Prelude

import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import Options.Applicative
    ( Parser
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , optional
    , progDesc
    , strOption
    , (<**>)
    )

data CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: FileOf "cluster-configs"
    , faucetFundsFile :: FileOf "faucet-funds"
    , clusterDir :: Maybe (FileOf "cluster")
    }
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions =
    execParser
        $ info
            ( CommandLineOptions
                <$> clusterConfigsDirParser
                <*> faucetFundsParser
                <*> clusterDirParser
                <**> helper
            )
            (progDesc "Local Cluster for testing")

clusterConfigsDirParser :: Parser (FileOf "cluster-configs")
clusterConfigsDirParser =
    FileOf
        <$> strOption
            ( long "cluster-configs"
                <> metavar "LOCAL_CLUSTER_CONFIGS"
                <> help "Path to the local cluster configuration directory"
            )

faucetFundsParser :: Parser (FileOf "faucet-funds")
faucetFundsParser =
    FileOf
        <$> strOption
            ( long "faucet-funds"
                <> metavar "FAUCET_FUNDS"
                <> help "Path to the faucet funds configuration file"
            )

clusterDirParser :: Parser (Maybe (FileOf "cluster"))
clusterDirParser = optional $
    FileOf
        <$> strOption
            ( long "cluster"
                <> metavar "LOCAL_CLUSTER"
                <> help "Path to the local cluster directory"
            )
