{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    ) where

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
    , progDesc
    , strOption
    , (<**>)
    )
import Prelude

newtype CommandLineOptions = CommandLineOptions
    {clusterConfigsDir :: FileOf "cluster-configs"}
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions =
    execParser
        $ info
            (fmap CommandLineOptions clusterConfigsDirParser <**> helper)
            (progDesc "Local Cluster for testing")

clusterConfigsDirParser :: Parser (FileOf "cluster-configs")
clusterConfigsDirParser =
    FileOf
        <$> strOption
            ( long "cluster-configs"
                <> metavar "LOCAL_CLUSTER_CONFIGS"
                <> help "Path to the local cluster configuration directory"
            )
