-- We intentionally specify more constraints than necessary for some exports.
{-# OPTIONS_GHC -Wno-redundant-constraints#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Data.Store (
    -- * Synopsis
    -- | 'Store' represents a facility for storing one value of a given type.
    --
    -- Typically, this facility is useful when we want to store
    -- a value __outside of RAM__, e.g. in a database file on the hard disk,
    -- because otherwise, we can just work with the Haskell value itself.

    -- * Store, definition
    -- ** Type
      Store (..)

    -- ** Properties
    -- $Properties

    -- *** Store Laws
    -- $StoreLaws

    -- *** Monad
    -- $StoreMonad

    -- *** updateS
    -- $updateS

    -- *** loadS, SomeException
    -- $EitherSomeException

    -- * Store, functions
    -- ** Helpers
    , updateLoad
    , loadWhenNothing

    -- ** Combinators
    , embedStore
    , pairStores
    , newCachedStore

    -- ** Testing
    , embedStore'
    , newStore, NotInitialized (..)
    ) where

import Prelude

import Control.Applicative
    ( liftA2 )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    , writeTVar
    )
import Control.Exception
    ( Exception, SomeException (..), toException )
import Control.Monad
    ( join )
import Control.Monad.Class.MonadThrow
    ( MonadEvaluate, MonadMask, MonadThrow, evaluate, finally, mask, throwIO )
import Data.Delta
    ( Delta (..), Embedding, Embedding' (..), Machine (..), inject, project )

{-------------------------------------------------------------------------------
    Store
-------------------------------------------------------------------------------}
{- |
A 'Store' is a storage facility for Haskell values of type @a ~ @'Base'@ da@.
Typical use cases are a file or a database on the hard disk.
-}
data Store m da = Store
    { -- | Load the value from the store into memory, or fail.
      -- This operation can be an expensive.
      loadS   :: m (Either SomeException (Base da))
      -- | Write a value to the store.
    , writeS  :: Base da -> m ()
      -- | Update the store efficiently by using a delta encoding @da@.
      -- The first argument may supply the current in-memory value
      -- for efficiency.
    , updateS
        :: Maybe (Base da) -- old value, for performance
        -> da -- delta to new value
        -> m () -- write new value
    }

{- $Properties
Any implementation of 'Store' is expected to satisfy the __properties__
specified in this section.
We make no attempt at enforcing these properties on the type-level.
However, the module "Test.Store" provides QuickCheck code for these
properties for automated testing.

The most important aspect of a 'Store' is that it has
many similarities with an 'Embedding'.
These similarities are captured in the __store laws__ discussed below.

The main difference to 'Embedding' is that storing a value in
a 'Store' has __side effects__. In particular,
access to the storage space is encapsulated in the monad.
This requires additional considerations with regards to e.g. exceptions
and concurrency.
-}

-- Note [StoreLaws]
{- $StoreLaws
The following properties characterize the most important aspects
of a 'Store':

1. The store __need not contain__ a properly formatted __value__.

    Loading a value from the store may fail, and this is why 'loadS'
    has an 'Either' result.
    For example, if the 'Store' represents
    a file on disk, then the file may corrupted or in an incompatible
    file format when first opened.
    In such a case of failure, the result 'Left'@ (e :: @'SomeException'@)@
    is returned, where the exception @e@ gives more information
    about the failure.

    However, loading a value after writing it should always succeed,
    we have

        > writeS s a >> loadS s  =  pure (Right a)

2. The store is __redundant__.

    Two stores with different contents may describe
    the same value of type @a@.
    For example, two files with different whitespace
    may describe the same JSON value.
    In general, we have

        > loadS s >>= either (const $ pure ()) (writeS s)  ≠  pure ()

3. Updating a store __commutes with 'apply'__.

    We have

        > updateS s (Just a) da >> loadS s  =  pure $ Right $ apply a da

    However, since the store is redundant, we often have

        > updateS s (Just a) da  ≠  writeS s (apply a da)
-}

-- Note [updateS]
{- $updateS

The function 'updateS' applies a delta to the content of the 'Store'.
Depending on the implementation of the 'Store', this operation may
require large parts of the content to be loaded into memory,
which is expensive.
In some use cases such as 'Data.DBVar.DBVar', the value is already available
in memory and can be used for executing the update.
For these cases, the __first argument__ of 'updateS'
__may__ provide the __in-memory value__.
We expect that the following property holds:

>   updateS s Nothing da
> =
>   loadS s >>= \(Right a) -> updateS s (Just a) da

The helper 'loadWhenNothing' is useful for handling this argument.

-}

{- $StoreMonad

The monad @m@ in 'Store'@ m da@ provides the storage space for the value.
Put differently, we like to think of @m@ as a
'Control.Monad.Trans.State.State' monad whose state contains the value.
However, this monad @m@ can have __additional effects__
such as exceptions, non-determinism, or concurrency,
and this complicates the specification significantly.
(In fact, the equality sign @=@ for the Store Laws has to be
interpreted "… equal effects as far as the 'Store' is concerned".
A correct approach to a specification would involve Hoare logic.)

We assume that the monad @m@ only has the effects __state__ and
__exceptions__ — we make no attempt at specifying how an implementation
should behave for concurrent usage of, say, 'updateS'.
This assumption ensures some composability of the 'Store' abstraction.
However, it also implies that choosing @m ~ @'Control.Monad.STM.STM'
results in specified semantics, whereas choosing @m ~ @'IO' can
result in unspecified behavior.
(TODO: Perhaps create a type class 'MonadSequential' to keep track
of this on the type level?)

More specifically, the interaction between 'Store' functions and
effects are as follows:

* __State__: The Store Laws presented above specify the essentials
of how the store state changes. However, this specification is not complete,
other "expected" rules such as

    > writeS s a >> writeS s b  =  writeS s b

    etc. should also hold.

* __Exceptions__:

    * 'loadS' should not throw a synchronous exception,
      but return 'Left' instead.
    * 'writeS' and 'loadS' should not throw synchronous exceptions.
      However, in case they do throw an exception,
      the contents of the 'Store' should be treated as corrupted,
      and 'loadS' should return 'Left' subsequently.

* __Concurrency__: We do not specify behavior under concurrent operation.
    However, concurrent access to a 'Store' is a frequent desire
    — but you will have to implement it yourself.

    One design pattern is to use a custom monad @m ~ MyMonad@
    that has a way of executing state changes atomically,

    > atomically :: MyMonad a -> IO a

    Specifically, @atomically@ either applies /all/ state changes,
    or /none/ of the state changes.
    For instance, SQL transactions can be used for this,
    see e.g. <https://www.sqlite.org/lang_transaction.html>.
    Then, you can implement a 'Store'@ MyMonad@ by composing smaller 'Store',
    and use @atomically@ in a scope where you want to use the 'Store'
    rather than implement it.

* __Non-determinism__ or other effects: Here be dragons.

-}

-- Note [EitherSomeException]
{- $EitherSomeException

In case of an __error case__, 'loadS' and 'load' return a failure value
of type 'SomeException' type.
This type is a disjoint sum of all possible
error types (that is, members of the 'Exception' class).

We could parametrize 'Store' by an additional type parameter @e@ representing
the possible error cases. However, we have opted to explore
a region of the design space where the number of type parameters
is kept to a minimum.

In fact, I would argue that making errors visible on the type level is not
very useful: we add much noise to the type level,
but we gain little type-safety in exchange.
Specifically, if we encounter an element of the 'SomeException' type that
we did not expect, we can always 'throw' it.
For example, consider the following code:

@
let ea :: Either SomeException ()
    ea = [..]
in
    case ea of
        Right _ -> "everything is ok"
        Left e -> case fromException e of
            Just (AssertionFailed _) -> "bad things happened"
            Nothing -> throw e
@

In this example, using the more specific type @ea :: Either AssertionFailed ()@
would have eliminated the 'Nothing' case.
However, this case has the sensible default value:
@throw e@, we rethrow the exception that we did not expect.
Ruling out this case on the type-level adds almost no value.
-}

{- HLINT ignore newStore "Use readTVarIO" -}
-- | An in-memory 'Store' from a mutable variable ('TVar').
-- Useful for testing.
newStore :: (Delta da, MonadSTM m) => m (Store m da)
newStore = do
    ref <- newTVarIO $ Left $ toException NotInitialized
    pure $ Store
        { loadS   = atomically $ readTVar ref
        , writeS  = atomically . writeTVar ref . Right
        , updateS = \_ -> atomically . modifyTVar' ref . fmap . apply
        }

-- | Failure that occurs when calling 'loadS' on a 'newStore' that is empty.
data NotInitialized = NotInitialized deriving (Eq, Show)
instance Exception NotInitialized

{-------------------------------------------------------------------------------
    Combinators
-------------------------------------------------------------------------------}
-- | Add a caching layer to a 'Store'.
--
-- Access to the underlying 'Store' is enforced to be sequential,
-- but the cache can be accessed in parallel.
-- FIXME: There is still a small race condition where the cache
-- could be written twice before it is filled. 🤔
-- TODO: Think about whether it is really necessary to handle concurrency here.
-- I think the answer is "yes", but only because the mutable variables
-- provided by the monad @m@ do not work together with e.g. SQL transactions.
newCachedStore
    :: forall m da. (Delta da, MonadSTM m, MonadThrow m, MonadEvaluate m)
    => Store m da -> m (Store m da)
newCachedStore Store{loadS,writeS,updateS} = do
    -- Lock that puts loadS, writeS and updateS into sequence
    islocked <- newTVarIO False
    let withLock :: forall b. m b -> m b
        withLock action = do
            atomically $ readTVar islocked >>= \case
                True  -> retry
                False -> writeTVar islocked True
            action `finally` atomically (writeTVar islocked False)

    -- Cache that need not be filled in the beginning
    cache    <- newTVarIO (Nothing :: Maybe (Base da))
    let writeCache ma = writeTVar cache ma

    -- Load the value from the Store only if it is not cached and
    -- nobody else is writing to the store.
    let load :: m (Either SomeException (Base da))
        load = join $ atomically $ do
            ma <- readTVar cache
            case ma of
                Nothing -> readTVar islocked >>= \case
                    True  -> retry  -- somebody is writing
                    False -> pure $ withLock $ do
                        ea <- loadS
                        case ea of
                            Left  e -> pure $ Left e
                            Right a -> do
                                atomically $ writeCache $ Just a
                                pure $ Right a
                Just a -> pure $ pure $ Right a

    pure $ Store
        { loadS = load
        , writeS = \a -> withLock $ do
            atomically $ writeCache (Just a)
            writeS a
        , updateS = updateLoad load throwIO $ \old delta -> withLock $ do
            new <- evaluate $ apply delta old
            atomically $ writeCache $ Just new
            updateS (Just old) delta
        }

-- | Store one type in the 'Store' of another type by using an 'Embedding'.
embedStore :: (MonadSTM m, MonadMask m, Delta da)
    => Embedding da db -> Store m db -> m (Store m da)
embedStore embed bstore = do
    -- For reasons of efficiency, we have to store the 'Machine'
    -- that is created within the 'Embedding'.
    machine <- newTVarIO Nothing
    let readMachine  = readTVarIO machine
        writeMachine = atomically . writeTVar machine . Just

    -- Operations of the result 'Store'.
    let load = loadS bstore >>= \case
            Left  e -> pure $ Left e
            Right b -> case project embed b of
                Left  e       -> pure $ Left e
                Right (a,mab) -> do
                    writeMachine mab
                    pure $ Right a
        write a = do
            let mab = inject embed a
            mask $ \restore -> do
                restore $ writeS bstore (state_ mab)
                writeMachine mab
        update = updateLoad load throwIO $ \a da -> do
            readMachine >>= \case
                Nothing   -> do -- we were missing the initial write
                    write (apply da a)
                Just mab1 -> do -- advance the machine by one step
                    let (db, mab2) = step_ mab1 (a,da)
                    mask $ \restore -> do
                        restore $ updateS bstore (Just $ state_ mab2) db
                        writeMachine mab2
    pure $ Store {loadS=load,writeS=write,updateS=update}


-- | Store one type in the 'Store' of another type by using an 'Embedding'.
--
-- Note: This function is exported for testing and documentation only,
-- use the more efficient 'embedStore' instead.
embedStore'
    :: (Monad m, MonadThrow m)
    => Embedding' da db -> Store m db -> Store m da
embedStore' Embedding'{load,write,update} Store{loadS,writeS,updateS} =
    let
        loadL =  (load =<<) <$> loadS
        updateL = \ma da -> case ma of
            Just a -> loadS >>= \case
                Left  _ -> pure ()
                Right b -> updateS (Just b) (update a b da)
            Nothing -> do
                ea <- loadL
                case ea of
                    Left  e -> throwIO e
                    Right a -> updateL (Just a) da
    in Store
        { loadS   = loadL
        , writeS  = writeS . write
        , updateS = updateL
        }

-- | Combine two 'Stores' into a 'Store' for pairs.
--
-- TODO: Handle the case where 'writeS' or 'updateS' throw an exception
-- and partially break the 'Store'.
pairStores :: Monad m => Store m da -> Store m db -> Store m (da, db)
pairStores sa sb = Store
    { loadS = liftA2 (,) <$> loadS sa <*> loadS sb
    , writeS = \(a,b) -> writeS sa a >> writeS sb b
    , updateS = \mi (da,db) ->
        case mi of
            Nothing -> updateS sa Nothing da >> updateS sb Nothing db
            Just (a,b) -> updateS sa (Just a) da >> updateS sb (Just b) db
    }

{-------------------------------------------------------------------------------
    Helpers
-------------------------------------------------------------------------------}
-- | Helper for implementing `updateS`
-- for the case where a value is not yet loaded.
updateLoad :: (Exception e, Monad m)
    => m (Either e t) -- ^ How to load the value.
    -> (e -> m b) -- ^ What to do with the error when loading the value.
    -> (t -> da -> m b) -- ^ What to do with the value.
    -> Maybe t -- ^ Value, maybe loaded, maybe not.
    -> da -- ^ Delta.
    -> m b
updateLoad load handle update' Nothing da = do
    ea <- load
    case ea of
        Left e -> handle e
        Right x -> update' x da
updateLoad _load _  update' (Just x) da = update' x da

-- | Helper for implementing `updateS`.
-- Call 'loadS' from a 'Store' if the value is not already given in memory.
loadWhenNothing
    :: (Monad m, MonadThrow m, Delta da)
    => Maybe (Base da) -> Store m da -> m (Base da)
loadWhenNothing (Just a) _ = pure a
loadWhenNothing Nothing store =
    loadS store >>= \case
        Left (SomeException e) -> throwIO e
        Right a -> pure a