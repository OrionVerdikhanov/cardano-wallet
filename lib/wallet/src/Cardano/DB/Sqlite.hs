{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# OPTIONS_GHC -fno-warn-deprecations #-}

{- HLINT ignore "Redundant flip" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- A wrapper for SQLite database connections, to be used with 'persistent'.
module Cardano.DB.Sqlite
    ( SqliteContext (..)
    , withSqliteContextFile
    , newInMemorySqliteContext
    , ForeignKeysSetting (..)

      -- * DB Connections
    , DBHandle
    , withDBHandle
    , dbConn
    , dbFile
    , dbBackend

      -- * Migrations
    , runManualOldMigrations
    , matchWrongVersionError
    , noAutoMigrations
    , noManualMigration

      -- * Helpers
    , chunkSize
    , dbChunked
    , dbChunkedFor
    , dbChunked'

      -- * Logging
    , DBLog (..)
    , ReadDBHandle
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.BM.Extra
    ( BracketLog
    , bracketTracer
    )
import Cardano.DB.Sqlite.ForeignKeys
    ( ForeignKeysSetting (..)
    , withForeignKeysDisabled
    )
import Cardano.DB.Sqlite.Migration.Old
    ( DBMigrationOldLog (..)
    , ManualMigration (..)
    , MatchMigrationError (..)
    , MigrationError (..)
    , noManualMigration
    )
import Cardano.Wallet.DB.Migration
    ( ErrWrongVersion (..)
    )
import Control.Lens
    ( strict
    , view
    )
import Control.Monad
    ( join
    )
import Control.Monad.Logger
    ( LogLevel (..)
    )
import Control.Monad.Reader
    ( ReaderT
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Control.Retry
    ( RetryStatus (..)
    , constantDelay
    , limitRetriesByCumulativeDelay
    , logRetries
    , recovering
    )
import Control.Tracer
    ( Tracer
    , contramap
    , traceWith
    )
import Data.Aeson
    ( ToJSON (..)
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( ($>)
    , (<&>)
    )
import Data.List.Split
    ( chunksOf
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Text.Lazy.Builder
    ( toLazyText
    )
import Data.Time.Clock
    ( NominalDiffTime
    )
import Database.Persist.EntityDef
    ( getEntityFields
    )
import Database.Persist.Sql
    ( LogFunc
    , Migration
    , PersistEntity (..)
    , PersistException
    , SqlPersistT
    , close'
    , runMigrationUnsafeQuiet
    , runSqlConn
    )
import Database.Persist.Sqlite
    ( SqlBackend
    , wrapConnection
    )
import Database.Sqlite
    ( SqliteException (SqliteException)
    )
import Fmt
    ( Buildable (..)
    , fmt
    , ordinalF
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Generics
    ( Generic
    )
import System.Environment
    ( lookupEnv
    )
import System.Log.FastLogger
    ( fromLogStr
    )
import UnliftIO.Compat
    ( handleIf
    )
import UnliftIO.Exception
    ( Exception
    , bracket
    , throwIO
    , tryJust
    )
import UnliftIO.MVar
    ( newMVar
    , withMVar
    , withMVarMasked
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Persist.Sql as Persist
import qualified Database.Sqlite as Sqlite

{-------------------------------------------------------------------------------
                            Sqlite connection set up
-------------------------------------------------------------------------------}

-- | 'SqliteContext' is a facility to run database queries.
newtype SqliteContext = SqliteContext
    { runQuery :: forall a. SqlPersistT IO a -> IO a
    }

newInMemorySqliteContext
    :: Tracer IO DBLog
    -> ManualMigration
    -> Migration
    -> ForeignKeysSetting
    -> IO (IO (), SqliteContext)
newInMemorySqliteContext tr manualMigrations autoMigration disableFK = do
    db <- newDBHandleInMemory tr
    throwLeft =<< runManualOldMigrations tr manualMigrations db
    throwLeft =<< runAutoMigration tr autoMigration db

    let observe :: forall a. IO a -> IO a
        observe = bracketTracer (contramap MsgRun tr)

    -- We still use a lock with the in-memory database to protect it from
    -- concurrent accesses and ensure database integrity in case where multiple
    -- threads would be reading/writing from/to it.
    lock <- newMVar (dbBackend db)
    let trFK = contramap MsgUpdatingForeignKeysSetting tr
        useForeignKeys :: IO a -> IO a
        useForeignKeys
            | disableFK == ForeignKeysDisabled =
                withForeignKeysDisabled trFK (dbConn db)
            | otherwise = id
        runQuery :: forall a. SqlPersistT IO a -> IO a
        runQuery cmd =
            withMVarMasked
                lock
                (observe . useForeignKeys . runSqlConn cmd)

    return (closeDBHandle tr db, SqliteContext{runQuery})

-- | Throw 'Left' as an exception in the monad.
throwLeft :: Exception e => Either e b -> IO b
throwLeft = \case
    Left e -> throwIO e
    Right b -> pure b

-- | Sets up query logging and timing, runs schema migrations if necessary and
-- provide a safe 'SqliteContext' for interacting with the database.
withSqliteContextFile
    :: Tracer IO DBLog
    -- ^ Logging
    -> FilePath
    -- ^ Database file
    -> ManualMigration
    -- ^ Manual migrations
    -> Migration
    -- ^ Auto migration
    -> (SqliteContext -> IO a)
    -> IO (Either MigrationError a)
withSqliteContextFile tr fp old auto action = runExceptT $ do
    ExceptT $ withDBHandle tr fp $ runManualOldMigrations tr old
    ExceptT $ withDBHandle tr fp $ runAutoMigration tr auto
    lift $ do
        lock <- newMVar ()
        withDBHandle tr fp $ \DBHandle{dbBackend} ->
            let
                -- Run a query on the open database,
                -- but retry on busy.
                runQuery :: SqlPersistT IO a -> IO a
                runQuery cmd =
                    observe
                        . retryOnBusy tr retryOnBusyTimeout
                        $ withMVar lock
                        $ const
                        $ runSqlConn cmd dbBackend
            in
                action (SqliteContext{runQuery})
  where
    observe :: IO a -> IO a
    observe = bracketTracer (contramap MsgRun tr)

{-------------------------------------------------------------------------------
    SQL connection life-cycle
    low level
-------------------------------------------------------------------------------}
data DBHandle = DBHandle
    { dbConn :: Sqlite.Connection
    , dbBackend :: SqlBackend
    , dbFile :: FilePath
    }

type ReadDBHandle m = ReaderT DBHandle m

-- | Acquire and release a 'DBHandle' from a file.
withDBHandle
    :: Tracer IO DBLog
    -> FilePath
    -> (DBHandle -> IO a)
    -> IO a
withDBHandle tr fp =
    bracket (newDBHandle tr fp) (closeDBHandleRetrying tr)

-- | Create a new 'DBHandle' from a file.
-- Needs to be closed explicitly.
newDBHandle
    :: Tracer IO DBLog
    -> FilePath
    -> IO DBHandle
newDBHandle tr dbFile = do
    traceWith tr $ MsgOpenSingleConnection dbFile
    dbConn <- Sqlite.open (T.pack dbFile)
    dbBackend <- wrapConnection dbConn (queryLogFunc tr)
    pure $ DBHandle{dbFile, dbConn, dbBackend}

-- | Create a new 'DBHandle' in memory.
-- Needs to be closed explicitly.
newDBHandleInMemory
    :: Tracer IO DBLog
    -> IO DBHandle
newDBHandleInMemory tr =
    newDBHandle tr ":memory:"

-- | Attempt to close the database connection and finalize database statements.
--
-- This function is idempotent: if the database connection has already been
-- closed, calling this function will exit without doing anything.
--
-- This function may still fail to close the connection due to the @SQLITE_BUSY@
-- exception.
closeDBHandle
    :: Tracer IO DBLog
    -> DBHandle
    -> IO ()
closeDBHandle tr DBHandle{dbBackend} = do
    close' dbBackend
        & handleIf
            isAlreadyClosed
            (traceWith tr . MsgIsAlreadyClosed . showT)
        & handleIf
            statementAlreadyFinalized
            (traceWith tr . MsgStatementAlreadyFinalized . showT)
  where
    isAlreadyClosed = \case
        -- Thrown when an attempt is made to close a connection that is already
        -- in the closed state:
        Sqlite.SqliteException Sqlite.ErrorMisuse _ _ -> True
        Sqlite.SqliteException{} -> False

    statementAlreadyFinalized = \case
        -- Thrown
        Persist.StatementAlreadyFinalized{} -> True
        Persist.Couldn'tGetSQLConnection{} -> False

    showT :: Show a => a -> Text
    showT = T.pack . show

-- | Like 'closeDBHandle',
-- but will retry repeatedly on @SQLITE_BUSY@ exception.
--
-- Will retry for up to a minute ('retryOnBusyTimeout').
closeDBHandleRetrying
    :: Tracer IO DBLog
    -> DBHandle
    -> IO ()
closeDBHandleRetrying tr db@DBHandle{dbFile} = do
    traceWith tr $ MsgCloseSingleConnection dbFile

    -- Hack for ADP-827: timeout earlier in integration tests.
    --
    -- There seem to be some concurrency problem causing persistent-sqlite to
    -- leak unfinalized statements, causing SQLITE_BUSY when we try to close the
    -- connection. In this case, retrying 2 or 60 seconds would have no
    -- difference.
    --
    -- But in production, the longer timeout isn't as much of a problem, and
    -- might be needed for windows.
    timeoutSec <-
        lookupEnv "CARDANO_WALLET_TEST_INTEGRATION" <&> \case
            Just _ -> 2
            Nothing -> retryOnBusyTimeout

    retryOnBusy tr timeoutSec $ closeDBHandle tr db

-- | Default timeout for `retryOnBusy`
retryOnBusyTimeout :: NominalDiffTime
retryOnBusyTimeout = 60

-- | Retry an action if the database yields an 'SQLITE_BUSY' error.
--
-- From <https://www.sqlite.org/rescode.html#busy>
--
--     The SQLITE_BUSY result code indicates that the database file could not be
--     written (or in some cases read) because of concurrent activity by some
--     other database connection, usually a database connection in a separate
--     process.
--
--     For example, if process A is in the middle of a large write transaction
--     and at the same time process B attempts to start a new write transaction,
--     process B will get back an SQLITE_BUSY result because SQLite only supports
--     one writer at a time. Process B will need to wait for process A to finish
--     its transaction before starting a new transaction. The sqlite3_busy_timeout()
--     and sqlite3_busy_handler() interfaces and the busy_timeout pragma are
--     available to process B to help it deal with SQLITE_BUSY errors.
retryOnBusy
    :: Tracer IO DBLog
    -- ^ Logging
    -> NominalDiffTime
    -- ^ Timeout
    -> IO a
    -- ^ Action to retry
    -> IO a
retryOnBusy tr timeout action =
    recovering
        policy
        [logRetries isBusy traceRetries]
        (\st -> action <* trace MsgRetryDone st)
  where
    policy = limitRetriesByCumulativeDelay usTimeout $ constantDelay (25 * ms)
    usTimeout = truncate (timeout * 1_000_000)
    ms = 1_000 -- microseconds in a millisecond
    isBusy (SqliteException name _ _) = pure (name == Sqlite.ErrorBusy)

    traceRetries retr _ = trace $ if retr then MsgRetry else MsgRetryGaveUp

    trace m RetryStatus{rsIterNumber} =
        traceWith tr
            $ MsgRetryOnBusy rsIterNumber m

{-------------------------------------------------------------------------------
    Database migrations
    old style and new style
-------------------------------------------------------------------------------}

noAutoMigrations :: Migration
noAutoMigrations = pure ()

runAutoMigration
    :: Tracer IO DBLog
    -> Migration
    -> DBHandle
    -> IO (Either MigrationError ())
runAutoMigration tr autoMigration DBHandle{dbConn, dbBackend} = do
    let executeAutoMigration =
            runSqlConn
                (runMigrationUnsafeQuiet autoMigration)
                dbBackend
        trFK = contramap MsgUpdatingForeignKeysSetting tr
    migrationResult <- withForeignKeysDisabled trFK dbConn $ do
        executeAutoMigration
            & tryJust (matchMigrationError @PersistException)
            & tryJust (matchMigrationError @SqliteException)
            & fmap join
    traceWith tr $ MsgMigrations $ length <$> migrationResult
    return $ migrationResult $> ()

runManualOldMigrations
    :: Tracer IO DBLog
    -> ManualMigration
    -> DBHandle
    -> IO (Either MigrationError ())
runManualOldMigrations tr manualMigration DBHandle{dbConn} = do
    let trFK = contramap MsgUpdatingForeignKeysSetting tr
    withForeignKeysDisabled trFK dbConn
        $ Right
            <$> (`executeManualMigration` dbConn) manualMigration

matchWrongVersionError :: ErrWrongVersion -> Maybe MigrationError
matchWrongVersionError =
    Just
        . MigrationError
        . view strict
        . toLazyText
        . build

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data DBLog
    = MsgMigrations (Either MigrationError Int)
    | MsgMigrationOld DBMigrationOldLog
    | MsgQuery Text Severity
    | MsgRun BracketLog
    | MsgOpenSingleConnection FilePath
    | MsgCloseSingleConnection FilePath
    | MsgDatabaseReset
    | MsgIsAlreadyClosed Text
    | MsgStatementAlreadyFinalized Text
    | MsgUpdatingForeignKeysSetting ForeignKeysSetting
    | MsgRetryOnBusy Int RetryLog
    deriving (Generic, Show, Eq, ToJSON)

data RetryLog = MsgRetry | MsgRetryGaveUp | MsgRetryDone
    deriving (Generic, Show, Eq, ToJSON)

instance HasPrivacyAnnotation DBLog
instance HasSeverityAnnotation DBLog where
    getSeverityAnnotation ev = case ev of
        MsgMigrations (Right 0) -> Debug
        MsgMigrations (Right _) -> Notice
        MsgMigrations (Left _) -> Error
        MsgMigrationOld msg -> getSeverityAnnotation msg
        MsgQuery _ sev -> sev
        MsgRun _ -> Debug
        MsgCloseSingleConnection _ -> Info
        MsgDatabaseReset -> Notice
        MsgIsAlreadyClosed _ -> Warning
        MsgStatementAlreadyFinalized _ -> Warning
        MsgUpdatingForeignKeysSetting{} -> Debug
        MsgRetryOnBusy n _
            | n <= 5 -> Debug
            | n <= 20 -> Info
            | otherwise -> Warning
        MsgOpenSingleConnection _ -> Debug

instance ToText DBLog where
    toText = \case
        MsgMigrations (Right 0) ->
            "No database migrations were necessary."
        MsgMigrations (Right n) ->
            fmt $ "" +|| n ||+ " migrations were applied to the database."
        MsgMigrations (Left err) ->
            "Failed to migrate the database: " <> getMigrationErrorMessage err
        MsgQuery stmt _ -> stmt
        MsgRun b ->
            "Running database action - " <> toText b
        MsgDatabaseReset ->
            "Non backward compatible database found. Removing old database \
            \and re-creating it from scratch. Ignore the previous error."
        MsgOpenSingleConnection fp ->
            "Opening single database connection (" +| fp |+ ")"
        MsgCloseSingleConnection fp ->
            "Closing single database connection (" +| fp |+ ")"
        MsgIsAlreadyClosed msg ->
            "Attempted to close an already closed connection: " <> msg
        MsgStatementAlreadyFinalized msg ->
            "Statement already finalized: " <> msg
        MsgMigrationOld msg -> toText msg
        MsgUpdatingForeignKeysSetting value ->
            mconcat
                [ "Updating the foreign keys setting to: "
                , T.pack $ show value
                , "."
                ]
        MsgRetryOnBusy n msg -> case msg of
            MsgRetry
                | n <= 10 ->
                    "Retrying db query because db was busy "
                        <> "for the " +| ordinalF n |+ " time."
                | n == 11 ->
                    "No more logs until it finishes..."
                | otherwise -> ""
            MsgRetryGaveUp -> "Gave up on retrying the db query."
            MsgRetryDone
                | n > 3 -> "DB query succeeded after " +| n |+ " attempts."
                | otherwise -> ""

-- | Produce a persistent 'LogFunc' backed by 'Tracer IO DBLog'
queryLogFunc :: Tracer IO DBLog -> LogFunc
queryLogFunc tr _loc _source level str = traceWith tr (MsgQuery msg sev)
  where
    -- Filter out parameters which appear after the statement semicolon.
    -- They will contain sensitive material that we don't want in the log.
    stmt = B8.takeWhile (/= ';') $ fromLogStr str
    msg = T.decodeUtf8 stmt
    sev = case level of
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Warning

{-------------------------------------------------------------------------------
                               Extra DB Helpers
-------------------------------------------------------------------------------}

-- | Convert a single DB "updateMany" (or similar) query into multiple
-- updateMany queries with smaller lists of values.
--
-- This is to prevent too many variables appearing in the SQL statement.
-- SQLITE_MAX_VARIABLE_NUMBER is 999 by default, and we will get a
-- "too many SQL variables" exception if that is exceeded.
--
-- We choose a conservative value 'chunkSize' << 999 because there can be
-- multiple variables per row updated.
dbChunked
    :: forall record b
     . PersistEntity record
    => ([record] -> SqlPersistT IO b)
    -> [record]
    -> SqlPersistT IO ()
dbChunked = dbChunkedFor @record

-- | Like 'dbChunked', but generalized for the case where the input list is not
-- the same type as the record.
dbChunkedFor
    :: forall record a b
     . PersistEntity record
    => ([a] -> SqlPersistT IO b)
    -> [a]
    -> SqlPersistT IO ()
dbChunkedFor = chunkedM (chunkSizeFor @record)

-- | Like 'dbChunked', but allows bundling elements with a 'Key'. Useful when
-- used with 'repsertMany'.
dbChunked'
    :: forall record b
     . PersistEntity record
    => ([(Key record, record)] -> SqlPersistT IO b)
    -> [(Key record, record)]
    -> SqlPersistT IO ()
dbChunked' = chunkedM (chunkSizeFor @record)

-- | Given an action which takes a list of items, and a list of items, run that
-- action multiple times with the input list cut into chunks.
chunkedM
    :: Monad m
    => Int
    -- ^ Chunk size
    -> ([a] -> m b)
    -- ^ Action to run on values
    -> [a]
    -- ^ The values
    -> m ()
chunkedM n f = mapM_ f . chunksOf n

-- | Maximum number of variables allowed in a single SQL statement
--
-- See also 'dbChunked'.
chunkSize :: Int
chunkSize = 999

-- | Size of chunks when inserting, updating or deleting many rows at once.
-- Worst-case is when all columns of a particular table gets updated / inserted,
-- thus to be safe we must ensure that we do not act on more than `chunkSize /
-- cols` variables.
--
-- See also 'dbChunked'.
chunkSizeFor :: forall record. PersistEntity record => Int
chunkSizeFor = chunkSize `div` cols
  where
    cols = length $ getEntityFields $ entityDef (Proxy @record)

-- TODO: Does getEntityFields differ from the past entityFields?
