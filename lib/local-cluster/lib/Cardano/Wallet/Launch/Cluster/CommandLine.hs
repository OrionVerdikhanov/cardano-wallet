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
    ( AbsDirOf
    , AbsFileOf
    , Absolutize (..)
    , DirOf
    , FileOf
    , mkAbsolutize
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
    , ReadM
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
    , str
    , (<**>)
    )
import Path
    ( Abs
    , parseSomeDir
    , parseSomeFile
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
    { clusterConfigsDir :: AbsDirOf "cluster-configs"
    , faucetFundsFile :: AbsFileOf "faucet-funds"
    , clusterDir :: Maybe (AbsDirOf "cluster")
    , monitoring :: Maybe Monitoring
    , clusterControl :: Maybe ClusterControl
    , clusterLogs :: Maybe (AbsFileOf "cluster-logs")
    , nodeToClientSocket :: AbsFileOf "node-to-client-socket"
    }
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = do
    absolutize :: Absolutize <- mkAbsolutize
    let options = clusterControlParser $ do
            clusterConfigsDir <- clusterConfigsDirParser absolutize
            faucetFundsFile <- faucetFundsParser absolutize
            clusterDir <- clusterDirParser absolutize
            clusterLogs <- clusterLogsParser absolutize
            monitoring <- monitoringParser
            nodeToClientSocket <- nodeToClientSocketParser absolutize
            pure $ \clusterControl -> CommandLineOptions{..}
    execParser
        $ info
            (options <**> helper)
            (progDesc "Local Cluster for testing")

nodeToClientSocketParser :: Absolutize -> Parser (AbsFileOf "node-to-client-socket")
nodeToClientSocketParser cwd =
    option
        (fileParser cwd)
        ( long "socket-path"
            <> metavar "NODE_TO_CLIENT_SOCKET"
            <> help "Path to the node-to-client socket"
        )

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

clusterConfigsDirParser
    :: Absolutize
    -> Parser (AbsDirOf "cluster-configs")
clusterConfigsDirParser cwd =
    option
        (dirParser cwd)
        ( long "cluster-configs"
            <> metavar "LOCAL_CLUSTER_CONFIGS"
            <> help "Path to the local cluster configuration directory"
        )

faucetFundsParser
    :: Absolutize
    -> Parser (AbsFileOf "faucet-funds")
faucetFundsParser cwd =
    option
        (fileParser cwd)
        ( long "faucet-funds"
            <> metavar "FAUCET_FUNDS"
            <> help "Path to the faucet funds configuration file"
        )

clusterDirParser
    :: Absolutize
    -> Parser (Maybe (AbsDirOf "cluster"))
clusterDirParser cwd =
    optional
        $ option
            (dirParser cwd)
            ( long "cluster"
                <> metavar "LOCAL_CLUSTER"
                <> help "Path to the local cluster directory"
            )

dirParser :: Absolutize -> ReadM (DirOf x Abs)
dirParser (Absolutize cwd) = do
    p <- str
    case parseSomeDir p of
        Nothing -> fail "Invalid directory path"
        Just p' -> pure $ cwd p'

fileParser :: Absolutize -> ReadM (FileOf x Abs)
fileParser (Absolutize cwd) = do
    p <- str
    case parseSomeFile p of
        Nothing -> fail "Invalid file path"
        Just p' -> pure $ cwd p'

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
renderControl (Just ClusterControl{..}) =
    [ "control"
    , "--control-port"
    , show clusterControlPort
    , "--pulling-mode"
    , renderPullingMode pullingMode
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

clusterLogsParser
    :: Absolutize
    -> Parser (Maybe (AbsFileOf "cluster-logs"))
clusterLogsParser cwd =
    optional
        $ option
            (fileParser cwd)
            ( long "cluster-logs"
                <> metavar "CLUSTER_LOGS"
                <> help "Path to the cluster logs file"
            )

-- renderCommandLineOptions :: CommandLineOptions -> [String]
-- renderCommandLineOptions CommandLineOptions{..} =
--     [ "--cluster-configs"
--     , pathOf clusterConfigsDir
--     , "--faucet-funds"
--     , pathOf faucetFundsFile
--     ]
--     <> case clusterDir of
--         Nothing -> []
--         Just clusterDir' ->
--             [ "--cluster"
--             , pathOf clusterDir'
--             ]
--     <> case monitoring of
--         Nothing -> []
--         Just (Monitoring port) ->
--             [ "--monitoring-port"
--             , show port
--             ]
--     <> case clusterControl of
--         Nothing -> []
--         Just control ->
--             renderControl (Just control)
--     <> case clusterLogs of
--         Nothing -> []
--         Just (FileOf logFile) ->
--             [ "--cluster-logs"
--             , pathOf logFile
--             ]
