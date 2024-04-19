{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch @cardano-node@.

module Cardano.Launcher.Node
    ( -- * Startup
      withCardanoNode
    , CardanoNodeConfig (..)
    , MaybeK (..)
    , maybeOfMaybeK
    , fmapMaybeK
    , Presence (..)
    , NodePort (..)

    -- * cardano-node Snockets
    , CardanoNodeConn
    , cardanoNodeConn
    , nodeSocketFile
    , isWindows

    -- * Helpers
    , nodeSocketPath
    ) where

import Prelude

import Cardano.Launcher
    ( LauncherLog
    , ProcessHandles (..)
    , StdStream (..)
    , withBackendCreateProcess
    )
import Cardano.Launcher.Logging
    ( traceHandle
    )
import Control.Monad
    ( (<=<)
    )
import Control.Tracer
    ( Tracer (..)
    , debugTracer
    , stdoutTracer
    )
import Data.Bifunctor
    ( first
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.List
    ( isPrefixOf
    )
import Data.Maybe
    ( fromMaybe
    , maybeToList
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import System.Environment
    ( getEnvironment
    )
import System.FilePath
    ( isValid
    , takeFileName
    )
import System.Info
    ( os
    )
import System.IO
    ( Handle
    )
import UnliftIO
    ( async
    , link
    )
import UnliftIO.Process
    ( CreateProcess (..)
    , proc
    )

import qualified Data.Text as T

-- | Parameters for connecting to the node.
newtype CardanoNodeConn = CardanoNodeConn FilePath
    deriving (Show, Eq)

-- | Gets the socket filename or pipe name from 'CardanoNodeConn'. Whether it's
-- a unix socket or named pipe depends on the value of 'isWindows'.
nodeSocketFile :: CardanoNodeConn -> FilePath
nodeSocketFile (CardanoNodeConn name) = name

-- | Produces a 'CardanoNodeConn' if the socket path or pipe name (depending on
-- 'isWindows') is valid.
cardanoNodeConn :: FilePath -> Either String CardanoNodeConn
cardanoNodeConn name
    | isWindows = if isValidWindowsPipeName name
        then Right $ CardanoNodeConn name
        else Left "Invalid pipe name."
    | otherwise = if isValid name
        then Right $ CardanoNodeConn name
        else Left "Invalid file path."

isWindows :: Bool
isWindows = os == "mingw32"

isValidWindowsPipeName :: FilePath -> Bool
isValidWindowsPipeName name = slashPipe `isPrefixOf` name
    && isValid (drop (length slashPipe) name)
  where
    slashPipe = "\\\\.\\pipe\\"

instance ToText CardanoNodeConn where
    toText = T.pack . nodeSocketFile

instance FromText CardanoNodeConn where
    fromText = first TextDecodingError . cardanoNodeConn . T.unpack

newtype NodePort = NodePort { unNodePort :: Int }
    deriving (Show, Eq)

-- | A subset of the @cardano-node@ CLI parameters, used for starting the
-- backend.
data CardanoNodeConfig d = CardanoNodeConfig
    { nodeDir :: FilePath
    , nodeConfigFile :: FilePath
    , nodeTopologyFile :: FilePath
    , nodeDatabaseDir :: FilePath
    , nodeDlgCertFile :: Maybe FilePath
    , nodeSignKeyFile :: Maybe FilePath
    , nodeOpCertFile :: Maybe FilePath
    , nodeKesKeyFile :: Maybe FilePath
    , nodeVrfKeyFile :: Maybe FilePath
    , nodePort :: Maybe NodePort
    , nodeLoggingHostname :: Maybe String
    , nodeExecutable :: Maybe FilePath
    , nodeOutputFile :: Maybe FilePath
    , nodeSocketPathFile :: MaybeK FilePath d
    }
    deriving (Show, Eq)

traceProcessHandles :: Tracer IO Text -> Maybe Handle -> IO ()
traceProcessHandles _ Nothing = pure ()
traceProcessHandles r (Just h) = link <=< async $ traceHandle r h

defaultOutTracer :: Maybe (Tracer IO Text) -> Tracer IO Text
defaultOutTracer Nothing = T.unpack >$< stdoutTracer
defaultOutTracer (Just tr) = tr

defaultErrTracer :: Maybe (Tracer IO Text) -> Tracer IO Text
defaultErrTracer Nothing = T.unpack >$< debugTracer
defaultErrTracer (Just tr) = tr

data Presence = Present | Absent

data MaybeK x a where
    NothingK :: MaybeK x Absent
    JustK :: x -> MaybeK x Present

deriving instance Show x => Show (MaybeK x a)
deriving instance Eq x => Eq (MaybeK x a)

fmapMaybeK :: (x -> y) -> MaybeK x a -> MaybeK y a
fmapMaybeK _ NothingK = NothingK
fmapMaybeK f (JustK x) = JustK (f x)

maybeOfMaybeK :: MaybeK x a -> Maybe x
maybeOfMaybeK NothingK = Nothing
maybeOfMaybeK (JustK x) = Just x

-- | Spawns a @cardano-node@ process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Tracer IO LauncherLog
    -- ^ Trace for subprocess control logging
    -> Maybe (Tracer IO Text)
    -- ^ Trace for cardano-node stdout
    -> Maybe (Tracer IO Text)
    -- ^ Trace for cardano-node stderr
    -> CardanoNodeConfig d
    -- ^ Configuration for the node
    -> (MaybeK CardanoNodeConn d -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO a
withCardanoNode tr nodeOutTr nodeErrTr cfg action = do
    let socketPath = fmapMaybeK nodeSocketPath $ nodeSocketPathFile cfg
    cp <- cardanoNodeProcess cfg $ maybeOfMaybeK socketPath
    withBackendCreateProcess tr cp
        $ \(ProcessHandles _ mout merr _) -> do
            traceProcessHandles (defaultOutTracer nodeOutTr) mout
            traceProcessHandles (defaultErrTracer nodeErrTr) merr
            action $ fmapMaybeK CardanoNodeConn socketPath

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess
    :: CardanoNodeConfig d
    -> Maybe FilePath
    -> IO CreateProcess
cardanoNodeProcess cfg socketPath = do
    myEnv <- getEnvironment
    let env' = ("CARDANO_NODE_LOGGING_HOSTNAME",) <$> nodeLoggingHostname cfg
    pure
        $ (proc (fromMaybe "cardano-node" $ nodeExecutable cfg) args)
            { env = Just $ maybeToList env' ++ myEnv
            , cwd = Just $ nodeDir cfg
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
  where
    args =
        [ "run"
        , "--config"
        , nodeConfigFile cfg
        , "--topology"
        , nodeTopologyFile cfg
        , "--database-path"
        , nodeDatabaseDir cfg
        ]
            <> maybe [] (\p -> ["--socket-path", p]) socketPath
            <> opt "--port" (show . unNodePort <$> nodePort cfg)
            ++ opt "--byron-signing-key" (nodeSignKeyFile cfg)
            ++ opt "--byron-delegation-certificate" (nodeDlgCertFile cfg)
            ++ opt "--shelley-operational-certificate" (nodeOpCertFile cfg)
            ++ opt "--shelley-kes-key" (nodeKesKeyFile cfg)
            ++ opt "--shelley-vrf-key" (nodeVrfKeyFile cfg)
            ++ ["+RTS", "-N4", "-RTS"]

    opt _ Nothing = []
    opt arg (Just val) = [arg, val]

-- | Generate a 'FilePath' for the @cardano-node@ domain socket/named pipe.
-- On Windows, the path is the filename at the end of the path
-- prefixed with @"\\\\.\\pipe\\"@.
nodeSocketPath
    :: FilePath
    -- ^ unix path
    -> FilePath
    -- ^ UNIX socket file path or Windows named pipe name
nodeSocketPath name
    | os == "mingw32" = mkWindowsPipeName $ takeFileName name
    | otherwise = name

mkWindowsPipeName :: FilePath -> FilePath
mkWindowsPipeName name = "\\\\.\\pipe\\" ++ name
