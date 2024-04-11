{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , Monitoring (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    , renderPullingMode
    , renderMonitoring
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
import Control.Monitoring
    ( MonitorState (..)
    )
import Network.Wai.Handler.Warp
    ( Port
    )
import Options.Applicative
    ( Parser
    , auto
    , command
    , eitherReader
    , execParser
    , help
    , helper
    , hsubparser
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , strOption
    , (<**>)
    )

data Monitoring = Monitoring
    { monitoringPort :: Port
    , pullingMode :: MonitorState
    }
    deriving stock (Show)

data CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: FileOf "cluster-configs"
    , faucetFundsFile :: FileOf "faucet-funds"
    , clusterDir :: Maybe (FileOf "cluster")
    , monitoring :: Maybe Monitoring
    , clusterLogs :: Maybe (FileOf "cluster-logs")
    }
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    let options = monitoringParser $ do
            clusterConfigsDir <- clusterConfigsDirParser
            faucetFundsFile <- faucetFundsParser
            clusterDir <- clusterDirParser
            clusterLogs <- clusterLogsParser
            pure $ \monitoring -> CommandLineOptions{..}
    execParser
        $ info
            (options <**> helper)
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
        ( long "monitoring-port"
            <> metavar "MONITORING_PORT"
            <> help "Port for the monitoring server"
        )
  where
    parse = do
        p <- auto
        unless (p `elem` validPorts)
            $ fail
            $ "Invalid port number. Must be inside: "
                ++ show (head validPorts)
                ++ ".."
                ++ show (last validPorts)
        pure p

monitorStateParser :: Parser MonitorState
monitorStateParser = do
    option
        parse
        ( long "pulling-mode"
            <> metavar "PULLING_MODE"
            <> help "Mode for the monitoring server"
        )
  where
    parse = eitherReader $ \case
        "pulling" -> pure PullingState
        "not-pulling" -> pure NotPullingState
        _ ->
            Left
                "Invalid pulling mode. \
                \ Must be either 'pulling' or 'not-pulling'"

renderPullingMode :: MonitorState -> String
renderPullingMode = \case
    NotPullingState -> "not-pulling"
    PullingState -> "pulling"

renderMonitoring :: Maybe Monitoring -> [String]
renderMonitoring (Just Monitoring{..})  =
    [ "monitoring"
    , "--monitoring-port", show monitoringPort
    , "--pulling-mode", renderPullingMode pullingMode
    ]
renderMonitoring Nothing =
    [ "no-monitoring"
    ]

monitoringParser :: Parser (Maybe Monitoring -> a) -> Parser a
monitoringParser f =
    hsubparser $ monitoringCommand <> noMonitoring
  where
    monitoringCommand =
        command "monitoring"
            $ info (f <*> yesMonitoringParser)
            $ progDesc "Enable monitoring"
    yesMonitoringParser =
        fmap Just
            $ Monitoring
                <$> portParser
                <*> monitorStateParser
    noMonitoring =
        command "no-monitoring"
            $ info (f <*> pure Nothing)
            $ progDesc "Disable monitoring"

clusterLogsParser :: Parser (Maybe (FileOf "cluster-logs"))
clusterLogsParser =
    optional
        $ FileOf
            <$> strOption
                ( long "cluster-logs"
                    <> metavar "LOCAL_CLUSTER_LOGS"
                    <> help "Path to the local cluster logs file"
                )
