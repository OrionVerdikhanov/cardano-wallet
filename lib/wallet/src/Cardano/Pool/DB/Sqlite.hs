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
module Cardano.Pool.DB.Sqlite
    ( SqliteContext (..)
    , withSqliteContextFile
    , newInMemorySqliteContext

      -- * Helpers
    , handleConstraint

      -- * Old-style Manual Migration
    , ManualMigration (..)
    , noManualMigration
    , MigrationError (..)
    , DBField (..)
    , tableName
    , fieldName
    , fieldType

      -- * Logging
    , DBLog (..)
    , foldMigrations
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
import Cardano.Wallet.DB.Migration
    ( ErrWrongVersion (..)
    )
import Control.Lens
    ( strict
    , view
    )
import Control.Monad
    ( join
    , void
    , when
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    )
import Control.Monad.Logger
    ( LogLevel (..)
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
import Data.List
    ( isInfixOf
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
    ( getEntityDBName
    )
import Database.Persist.Names
    ( EntityNameDB (..)
    , unFieldNameDB
    )
import Database.Persist.Sql
    ( EntityField
    , LogFunc
    , Migration
    , PersistEntity (..)
    , PersistException
    , SqlPersistT
    , SqlType (..)
    , close'
    , fieldDB
    , fieldSqlType
    , runMigrationUnsafeQuiet
    , runSqlConn
    )
import Database.Persist.Sqlite
    ( SqlBackend
    , wrapConnection
    )
import Database.Sqlite
    ( Error (ErrorConstraint)
    , SqliteException (SqliteException)
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
    , bracket_
    , handleJust
    , tryJust
    )
import UnliftIO.MVar
    ( newMVar
    , withMVar
    , withMVarMasked
    )

import qualified Data.Aeson as Aeson
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

-- | Run an action, and convert any Sqlite constraints exception into the given
-- error result. No other exceptions are handled.
handleConstraint :: MonadUnliftIO m => e -> m a -> m (Either e a)
handleConstraint e = handleJust select handler . fmap Right
  where
    select (SqliteException ErrorConstraint _ _) = Just ()
    select _ = Nothing
    handler = const . pure . Left $ e

newInMemorySqliteContext
    :: Tracer IO DBLog
    -> ManualMigration
    -> Migration
    -> IO (IO (), SqliteContext)
newInMemorySqliteContext tr manualMigrations autoMigration = do
    conn <- Sqlite.open ":memory:"
    executeManualMigration manualMigrations conn
    unsafeBackend <- wrapConnection conn (queryLogFunc tr)
    void $ runSqlConn (runMigrationUnsafeQuiet autoMigration) unsafeBackend

    let observe :: forall a. IO a -> IO a
        observe = bracketTracer (contramap MsgRun tr)

    -- We still use a lock with the in-memory database to protect it from
    -- concurrent accesses and ensure database integrity in case where multiple
    -- threads would be reading/writing from/to it.
    lock <- newMVar unsafeBackend
    let runQuery :: forall a. SqlPersistT IO a -> IO a
        runQuery cmd =
            withMVarMasked
                lock
                (observe . runSqlConn cmd)

    return (close' unsafeBackend, SqliteContext{runQuery})

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
    -> (Tracer IO DBLog -> FilePath -> IO ())
    -- ^ New style migrations
    -> (SqliteContext -> IO a)
    -> IO (Either MigrationError a)
withSqliteContextFile tr fp old auto new action = do
    migrationResult <- runAllMigrations tr fp old auto new
    case migrationResult of
        Left e -> pure $ Left e
        Right{} -> do
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
                    Right <$> action (SqliteContext{runQuery})
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

withDBHandle
    :: Tracer IO DBLog
    -> FilePath
    -> (DBHandle -> IO a)
    -> IO a
withDBHandle tr fp =
    bracket (newDBHandle tr fp) (destroyDBHandle tr)

newDBHandle
    :: Tracer IO DBLog
    -> FilePath
    -> IO DBHandle
newDBHandle tr dbFile = do
    traceWith tr $ MsgOpenSingleConnection dbFile
    dbConn <- Sqlite.open (T.pack dbFile)
    dbBackend <- wrapConnection dbConn (queryLogFunc tr)
    pure $ DBHandle{dbFile, dbConn, dbBackend}

-- | Finalize database statements and close the database connection.
-- If the database connection is still in use, it will retry for up to a minute,
-- to let other threads finish up.
--
-- This function is idempotent: if the database connection has already been
-- closed, calling this function will exit without doing anything.
destroyDBHandle
    :: Tracer IO DBLog
    -> DBHandle
    -> IO ()
destroyDBHandle tr DBHandle{dbFile, dbBackend = sqlBackend} = do
    traceWith tr (MsgCloseSingleConnection dbFile)

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

    retryOnBusy tr timeoutSec (close' sqlBackend)
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
    Foreign key settings
-------------------------------------------------------------------------------}

-- | Run the given task in a context where foreign key constraints are
--   /temporarily disabled/, before re-enabling them.
withForeignKeysDisabled
    :: Tracer IO DBLog
    -> Sqlite.Connection
    -> IO a
    -> IO a
withForeignKeysDisabled t c =
    bracket_
        (updateForeignKeysSetting t c ForeignKeysDisabled)
        (updateForeignKeysSetting t c ForeignKeysEnabled)

-- | Specifies whether or not foreign key constraints are enabled, equivalent
--   to the Sqlite 'foreign_keys' setting.
--
-- When foreign key constraints are /enabled/, the database will enforce
-- referential integrity, and cascading deletes are enabled.
--
-- When foreign keys constraints are /disabled/, the database will not enforce
-- referential integrity, and cascading deletes are disabled.
--
-- See the following resource for more information:
-- https://www.sqlite.org/foreignkeys.html#fk_enable
data ForeignKeysSetting
    = -- | Foreign key constraints are /enabled/.
      ForeignKeysEnabled
    | -- | Foreign key constraints are /disabled/.
      ForeignKeysDisabled
    deriving (Eq, Generic, ToJSON, Show)

-- | Read the current value of the Sqlite 'foreign_keys' setting.
readForeignKeysSetting :: Sqlite.Connection -> IO ForeignKeysSetting
readForeignKeysSetting connection = do
    query <- Sqlite.prepare connection "PRAGMA foreign_keys"
    state <- Sqlite.step query >> Sqlite.columns query
    Sqlite.finalize query
    case state of
        [Persist.PersistInt64 0] -> pure ForeignKeysDisabled
        [Persist.PersistInt64 1] -> pure ForeignKeysEnabled
        unexpectedValue ->
            error
                $ mconcat
                    [ "Unexpected result when querying the current value of "
                    , "the Sqlite 'foreign_keys' setting: "
                    , show unexpectedValue
                    , "."
                    ]

-- | Update the current value of the Sqlite 'foreign_keys' setting.
updateForeignKeysSetting
    :: Tracer IO DBLog
    -> Sqlite.Connection
    -> ForeignKeysSetting
    -> IO ()
updateForeignKeysSetting trace connection desiredValue = do
    traceWith trace $ MsgUpdatingForeignKeysSetting desiredValue
    query <-
        Sqlite.prepare connection
            $ "PRAGMA foreign_keys = " <> valueToWrite <> ";"
    _ <- Sqlite.step query
    Sqlite.finalize query
    finalValue <- readForeignKeysSetting connection
    when (desiredValue /= finalValue)
        $ error
        $ mconcat
            [ "Unexpected error when updating the value of the Sqlite "
            , "'foreign_keys' setting. Attempted to write the value "
            , show desiredValue
            , " but retrieved the final value "
            , show finalValue
            , "."
            ]
  where
    valueToWrite = case desiredValue of
        ForeignKeysEnabled -> "ON"
        ForeignKeysDisabled -> "OFF"

{-------------------------------------------------------------------------------
    Database migrations
    old style and new style
-------------------------------------------------------------------------------}

-- | Error type for when migrations go wrong after opening a database.
newtype MigrationError = MigrationError
    {getMigrationErrorMessage :: Text}
    deriving (Show, Eq, Generic, ToJSON)

instance Exception MigrationError

class Exception e => MatchMigrationError e where
    -- | Exception predicate for migration errors.
    matchMigrationError :: e -> Maybe MigrationError

instance MatchMigrationError PersistException where
    matchMigrationError e
        | mark `isInfixOf` msg = Just $ MigrationError $ T.pack msg
        | otherwise = Nothing
      where
        msg = show e
        mark = "Database migration: manual intervention required."

instance MatchMigrationError SqliteException where
    matchMigrationError (SqliteException ErrorConstraint _ msg) =
        Just $ MigrationError msg
    matchMigrationError _ =
        Nothing

matchWrongVersionError :: ErrWrongVersion -> Maybe MigrationError
matchWrongVersionError =
    Just
        . MigrationError
        . view strict
        . toLazyText
        . build

-- | Encapsulates an old-style manual migration action
--   (or sequence of actions) to be
--   performed immediately after an SQL connection is initiated.
newtype ManualMigration = ManualMigration
    {executeManualMigration :: Sqlite.Connection -> IO ()}

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
    migrationResult <- withForeignKeysDisabled tr dbConn $ do
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
    withForeignKeysDisabled tr dbConn
        $ Right
            <$> (`executeManualMigration` dbConn) manualMigration

runManualNewMigrations
    :: Tracer IO DBLog
    -> FilePath
    -> (Tracer IO DBLog -> FilePath -> IO ())
    -> IO (Either MigrationError ())
runManualNewMigrations tr fp newMigrations =
    newMigrations tr fp
        & tryJust matchWrongVersionError

runAllMigrations
    :: Tracer IO DBLog
    -> FilePath
    -> ManualMigration
    -> Migration
    -> (Tracer IO DBLog -> FilePath -> IO ())
    -> IO (Either MigrationError ())
runAllMigrations tr fp old auto new = runExceptT $ do
    ExceptT $ withDBHandle tr fp $ runManualOldMigrations tr old
    ExceptT $ withDBHandle tr fp $ runAutoMigration tr auto
    ExceptT $ runManualNewMigrations tr fp new

{-------------------------------------------------------------------------------
    Database migration helpers
-------------------------------------------------------------------------------}

noManualMigration :: ManualMigration
noManualMigration = ManualMigration $ const $ pure ()

foldMigrations :: [Sqlite.Connection -> IO ()] -> ManualMigration
foldMigrations ms = ManualMigration $ \conn -> mapM_ ($ conn) ms

data DBField where
    DBField
        :: forall record typ
         . (PersistEntity record)
        => EntityField record typ
        -> DBField

tableName :: DBField -> Text
tableName (DBField (_ :: EntityField record typ)) =
    unEntityNameDB $ getEntityDBName $ entityDef (Proxy @record)

fieldName :: DBField -> Text
fieldName (DBField field) =
    unFieldNameDB $ fieldDB $ persistFieldDef field

fieldType :: DBField -> Text
fieldType (DBField field) =
    showSqlType $ fieldSqlType $ persistFieldDef field

showSqlType :: SqlType -> Text
showSqlType = \case
    SqlString -> "VARCHAR"
    SqlInt32 -> "INTEGER"
    SqlInt64 -> "INTEGER"
    SqlReal -> "REAL"
    SqlDay -> "DATE"
    SqlTime -> "TIME"
    SqlDayTime -> "TIMESTAMP"
    SqlBlob -> "BLOB"
    SqlBool -> "BOOLEAN"
    SqlOther t -> t
    SqlNumeric precision scale ->
        T.concat
            [ "NUMERIC("
            , T.pack (show precision)
            , ","
            , T.pack (show scale)
            , ")"
            ]

instance Show DBField where
    show field = T.unpack (tableName field <> "." <> fieldName field)

instance Eq DBField where
    field0 == field1 = show field0 == show field1

instance ToJSON DBField where
    toJSON = Aeson.String . T.pack . show

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data DBLog
    = MsgMigrations (Either MigrationError Int)
    | MsgQuery Text Severity
    | MsgRun BracketLog
    | MsgOpenSingleConnection FilePath
    | MsgCloseSingleConnection FilePath
    | MsgDatabaseReset
    | MsgIsAlreadyClosed Text
    | MsgStatementAlreadyFinalized Text
    | MsgManualMigrationNeeded DBField Text
    | MsgExpectedMigration DBLog
    | MsgManualMigrationNotNeeded DBField
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
        MsgQuery _ sev -> sev
        MsgRun _ -> Debug
        MsgCloseSingleConnection _ -> Info
        MsgExpectedMigration _ -> Debug
        MsgDatabaseReset -> Notice
        MsgIsAlreadyClosed _ -> Warning
        MsgStatementAlreadyFinalized _ -> Warning
        MsgManualMigrationNeeded{} -> Notice
        MsgManualMigrationNotNeeded{} -> Debug
        MsgUpdatingForeignKeysSetting{} -> Debug
        MsgRetryOnBusy n _
            | n <= 1 -> Debug
            | n <= 3 -> Notice
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
        MsgExpectedMigration msg -> "Expected: " <> toText msg
        MsgManualMigrationNeeded field value ->
            mconcat
                [ tableName field
                , " table does not contain required field '"
                , fieldName field
                , "'. "
                , "Adding this field with a default value of "
                , value
                , "."
                ]
        MsgManualMigrationNotNeeded field ->
            mconcat
                [ tableName field
                , " table already contains required field '"
                , fieldName field
                , "'."
                ]
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
