{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Streaming.Callbacks
    ( -- * Type interfaces
      Domains (..)

      -- * Callbacks extraction
    , ProductOf (..)
    , pattern (:*)
    , WhatK (..)
    , WhatT (..)

      -- * Stream
    , Element (..)
    , ElementStream
    , newElementStream

      -- * Cell
    , Cell (..)
    , NewBuffer
    , newTMVarBuffer
    , newTBQueueBuffer
    )
where

import Prelude hiding
    ( take
    )

import Control.Concurrent.Class.MonadSTM
    ( MonadSTM (..)
    , newTBQueueIO
    )
import Control.Monad.Fix
    ( fix
    )
import Data.Kind
    ( Type
    )
import Numeric.Natural
    ( Natural
    )
import Streaming
    ( Stream
    , effect
    , wrap
    )

-- | A request that stores an input and a callback to return an output.
data Request m a b = Request a (b -> m ())

-- | Domain and codomain of the callback.
data Domains a b = Domains a b

-- | A generic functor serves as an element to store pure requests in a stream.
-- The 'x' is necessary to store the stream continuation into.
--
-- Notice that the 'Stream' data type cleverly differentiates between pure
-- layers and effectful layers. This allows us to store a pure request instead
-- of a 'Request' data type.
--
-- The stream producer will handle the composition of the pure layer with the
-- effectful layer to store the 'Request' data type. The net result is that the
-- parameter 'm' does not appear in the 'ElementLayer' data type.
-- Because 'a' and 'b' are existentially quantified, the 'w a b` has to witness
-- the types 'a' and 'b'. Tipically you will use a GADT to witness the types that
-- will be pattern matched on your side to disambiguate the types.
data Element w x where
    Element :: w a b -> a -> (b -> x) -> Element w x

instance Functor (Element as) where
    fmap g (Element w a f) = Element w a (g . f)

-- | A stream of Element that can be pulled from.
type ElementStream m as = Stream (Element as) m

-- | `fmap snd` at the type level.
type family Codomains as where
    Codomains '[] = '[]
    Codomains (Domains _a b ': as) = b ': Codomains as

-- | A cell that can be taken from and put into. We instroduce this abstraction
-- to avoid binding the implementation to IO / MVar.
data Cell m a = Cell
    { put :: a -> m ()
    , take :: m a
    }

-- | A function that creates a new cell.
type NewBuffer m = forall a. m (Cell m a)

-- | Create a new cell using an MVar.
newTMVarBuffer :: NewBuffer IO
newTMVarBuffer = do
    var <- newEmptyTMVarIO
    pure
        $ Cell
            { put = atomically . putTMVar var
            , take = atomically $ takeTMVar var
            }

-- | Create a new cell using a TBQueue.
newTBQueueBuffer
    :: Natural
    -- ^ size of the queue
    -> NewBuffer IO
newTBQueueBuffer size = do
    queue <- newTBQueueIO size
    pure
        $ Cell
            { put = atomically . writeTBQueue queue
            , take = atomically $ readTBQueue queue
            }

-- | Create the callbacks to transform a push service into a pull service and
-- the relative stream to pull from.
newElementStream
    :: (MkRequestSumCells (Codomains fs), MkCallback fs, Monad m)
    => ProductOf WitnessT m w fs
    -> NewBuffer m
    -> m (ElementStream m w x, ProductOf CallbackT m w fs)
newElementStream ws newCell = do
    requests <- newCell
    outputs <- mkVar newCell
    pure (mkProducer requests, mkCallback ws requests outputs id)

-- | A kind to distinguish between witness and callback
data WhatK = WitnessT | CallbackT

-- | A type to distinguish between witness and callback
data WhatT w m a b e where
    Witness :: w a b -> WhatT w m a b 'WitnessT
    Callback :: (a -> m b) -> WhatT w m a b 'CallbackT

-- | A generic record specially tailored for fields that are of the form
-- `WhatT w m a b f`.
data ProductOf f (m :: Type -> Type) w abs where
    Nil :: ProductOf f m w '[]
    One
        :: WhatT w m a b f
        -> ProductOf f m w abs
        -> ProductOf f m w (Domains a b ': abs)

-- | An infix version of the 'One'
pattern (:*)
    :: WhatT w m a b f
    -> ProductOf f m w abs
    -> ProductOf f m w (Domains a b ': abs)
pattern x :* xs = One x xs

infixr 5 :*

-- | A generic record specially tailored for fields that are of the form `f a`.
-- Reusing ProductOf seems hard.
data ProductOfF f as where
    NilF :: ProductOfF f '[]
    OneF :: f a -> ProductOfF f as -> ProductOfF f (a : as)

-- | A generic sum type specially tailored for fields that are of the form
-- `f m a b`.
data SumOf w f (m :: Type -> Type) abs where
    This :: w a b -> f m a b -> SumOf w f m (Domains a b ': abs)
    That :: SumOf w f m abs -> SumOf w f m (a ': abs)

-- | A generic sum type specially tailored for fields that are requests
type RequestSum m w fs = SumOf w Request m fs

-- | Create a streams out of requests blocked in a Cell.
mkProducer :: Monad m => Cell m (RequestSum m w fs) -> ElementStream m w x
mkProducer requests = fix $ \producer -> effect $ do
    request <- take requests
    pure $ wrap $ consLayer producer request

-- | Cons a request to the stream by mutating it into an Element.
consLayer
    :: Monad m
    => ElementStream m w x
    -- ^ the stream to loop into
    -> RequestSum m w fs
    -- ^ the request to cons
    -> Element w (ElementStream m w x)
consLayer producer = \case
    This w (Request a k) ->
        Element w a $ \o -> effect $ do
            k o
            pure producer
    That s -> consLayer producer s

-- | A generic record holding one Cell in each field to store the outputs
-- of the callbacks.
type Outputs m os = ProductOfF (Cell m) os

-- | Create a record of Cells to store the outputs of the callbacks.
class MkRequestSumCells os where
    mkVar :: Monad m => NewBuffer m -> m (Outputs m os)

instance MkRequestSumCells '[] where
    mkVar :: Monad m => NewBuffer m -> m (Outputs m '[])
    mkVar _ = pure NilF

instance MkRequestSumCells os => MkRequestSumCells (o ': os) where
    mkVar :: Monad m => NewBuffer m -> m (Outputs m (o : os))
    mkVar newCell = do
        var <- newCell
        OneF var <$> mkVar newCell

-- | Create a record of callbacks to that uses
-- * a common input Cell to store all requests types as a generic sum
-- * a record of Cells to store the outputs of the callbacks.
class MkCallback fs where
    mkCallback
        :: Monad m
        => ProductOf WitnessT m w fs
        -> Cell m (RequestSum m w as)
        -- ^ requests for the stream consumer
        -> ProductOfF (Cell m) (Codomains fs)
        -> (RequestSum m w fs -> RequestSum m w as)
        -> ProductOf CallbackT m w fs

instance MkCallback '[] where
    mkCallback
        :: ProductOf WitnessT m w '[]
        -> Cell m (RequestSum m w as)
        -> ProductOfF (Cell m) (Codomains '[])
        -> (RequestSum m w '[] -> RequestSum m w as)
        -> ProductOf CallbackT m w '[]
    mkCallback _ _ _ _ = Nil

instance MkCallback fs => MkCallback (Domains a b ': fs) where
    mkCallback
        :: Monad m
        => ProductOf WitnessT m w (Domains a b ': fs)
        -> Cell m (RequestSum m w as)
        -> ProductOfF (Cell m) (Codomains (Domains a b : fs))
        -> (RequestSum m w (Domains a b : fs) -> RequestSum m w as)
        -> ProductOf CallbackT m w (Domains a b : fs)
    mkCallback
        (One (Witness w) ws)
        inputs
        (OneF output outputs)
        f =
            One
                ( Callback $ \i -> do
                    put inputs
                        $ f
                        $ This w
                        $ Request i
                        $ put output
                    take output
                )
                $ mkCallback ws inputs outputs (f . That)
