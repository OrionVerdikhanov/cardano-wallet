{-# LANGUAGE ApplicativeDo #-}
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
import Cardano.Wallet.Network.Ports
    ( validPorts
    )
import Control.Monad
    ( unless
    )
import Network.Wai.Handler.Warp
    ( Port
    )
import Options.Applicative
    ( Parser
    , auto
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , strOption
    , (<**>)
    )

data CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: FileOf "cluster-configs"
    , faucetFundsFile :: FileOf "faucet-funds"
    , clusterDir :: Maybe (FileOf "cluster")
    , monitoringPort :: Port
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
                <*> portParser
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
clusterDirParser =
    optional
        $ FileOf
            <$> strOption
                ( long "cluster"
                    <> metavar "LOCAL_CLUSTER"
                    <> help "Path to the local cluster directory"
                )

portParser :: Parser Port
portParser = do
    option
        parse
        ( long "monitor-port"
            <> metavar "MONITOR_PORT"
            <> help "Port for the monitoring server"
        )
    where parse = do
            p <- auto
            unless (p `elem` validPorts) $
                fail $ "Invalid port number. Must be inside: " ++
                    show (head validPorts) ++ ".." ++ show (last validPorts)
            pure p
