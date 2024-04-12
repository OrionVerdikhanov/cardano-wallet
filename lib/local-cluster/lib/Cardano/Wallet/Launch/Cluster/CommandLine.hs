{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Launch.Cluster.CommandLine
    ( CommandLineOptions (..)
    , ClusterControl (..)
    , Monitoring (..)
    , parseCommandLineOptions
    , clusterConfigsDirParser
    , renderPullingMode
    , renderControl
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

data ClusterControl = ClusterControl
    { clusterControlPort :: Port
    , pullingMode :: MonitorState
    }
    deriving stock (Show)

newtype Monitoring = Monitoring
    { monitoringPort :: Port
    }
    deriving stock (Show)

data CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: FileOf "cluster-configs"
    , faucetFundsFile :: FileOf "faucet-funds"
    , clusterDir :: Maybe (FileOf "cluster")
    , monitoring :: Maybe Monitoring
    , clusterControl :: Maybe ClusterControl
    , clusterLogs :: Maybe (FileOf "cluster-logs")
    }
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    let options = clusterControlParser $ do
            clusterConfigsDir <- clusterConfigsDirParser
            faucetFundsFile <- faucetFundsParser
            clusterDir <- clusterDirParser
            clusterLogs <- clusterLogsParser
            monitoring <- monitoringParser
            pure $ \clusterControl -> CommandLineOptions{..}
    execParser
        $ info
            (options <**> helper)
            (progDesc "Local Cluster for testing")

monitoringParser :: Parser (Maybe Monitoring)
monitoringParser = optional $ Monitoring <$> httpPortParser

httpPortParser :: Parser Port
httpPortParser = do
    option
        parse
        ( long "monitoring-port"
            <> metavar "MONITORING_PORT"
            <> help "Port for the monitoring HTTP server"
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

tcpPortParser :: Parser Port
tcpPortParser = do
    option
        parse
        ( long "control-port"
            <> metavar "CONTROL_PORT"
            <> help "Port for the TCP control server"
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
            <> help "Mode for the control server"
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

renderControl :: Maybe ClusterControl -> [String]
renderControl (Just ClusterControl{..})  =
    [ "control"
    , "--control-port", show clusterControlPort
    , "--pulling-mode", renderPullingMode pullingMode
    ]
renderControl Nothing =
    [ "no-control"
    ]

clusterControlParser :: Parser (Maybe ClusterControl -> a) -> Parser a
clusterControlParser f =
    hsubparser $ controlCommand <> noControl
  where
    controlCommand =
        command "control"
            $ info (f <*> yesControlParser)
            $ progDesc "Enable control"
    yesControlParser =
        fmap Just
            $ ClusterControl
                <$> tcpPortParser
                <*> monitorStateParser
    noControl =
        command "no-control"
            $ info (f <*> pure Nothing)
            $ progDesc "Disable control"

clusterLogsParser :: Parser (Maybe (FileOf "cluster-logs"))
clusterLogsParser =
    optional
        $ FileOf
            <$> strOption
                ( long "cluster-logs"
                    <> metavar "LOCAL_CLUSTER_LOGS"
                    <> help "Path to the local cluster logs file"
                )
