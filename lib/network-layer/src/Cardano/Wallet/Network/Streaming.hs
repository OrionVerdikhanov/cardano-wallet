{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Network.Streaming
    ( -- * ChainStream creation
      ChainStream
    , withStreamingFromBlockChain

      -- * ChainStream manipulation
    , forConsensusS
    , eraBlockS
    , eraTxS
    , forChainStream
    , scanChainStream
    , CallbackLogs (..)
    , mapMChainStream
    , printChainStream
    )
where

import Prelude

import Cardano.Wallet.Network
    ( ChainFollowLog
    , ChainFollower (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Network.Rollback.ChainPoints
    ( ChainPoints (..)
    )
import Cardano.Wallet.Read
    ( BHeader
    , Block
    , ChainPoint (..)
    , ChainTip
    , ConsensusBlock
    , EraValue
    , IsEra (..)
    , Tx
    , applyEraFunValue
    , chainPointFromChainTip
    , fromConsensusBlock
    , getEraTransactions
    , sequenceEraValue
    , (:*:) (..)
    , (:.:) (..)
    )
import Cardano.Wallet.Read.Block.BHeader
    ( getEraBHeader
    )
import Control.Monad.Fix
    ( fix
    )
import Control.Monad.Trans.Cont
    ( ContT (..)
    )
import Control.Tracer
    ( Tracer
    )
import Data.Foldable
    ( toList
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , StandardCrypto
    )
import Streaming
    ( MFunctor (..)
    , MonadIO (..)
    , MonadTrans (..)
    , Of (..)
    , Stream
    )
import Streaming.Callbacks
    ( Domains
    , Element (..)
    , ElementStream
    , NewBuffer
    , ProductOf (Nil)
    , WhatK (..)
    , WhatT (..)
    , newElementStream
    , pattern (:*)
    )
import UnliftIO.Async
    ( withAsync
    )

import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S

data ChainWitness e a b where
    Forward :: ChainWitness e (ChainTip, e) ()
    Backward :: ChainWitness e ChainPoint ChainPoint
    Points :: ChainWitness e () [ChainPoint]

chainWitnesses
    :: ProductOf
        WitnessT
        m
        (ChainWitness e)
        '[ Domains (ChainTip, e) ()
         , Domains ChainPoint ChainPoint
         , Domains () [ChainPoint]
         ]
chainWitnesses =
    Witness Forward :* Witness Backward :* Witness Points :* Nil

type ChainStream e = ElementStream IO ((ChainWitness e))

-- | Expose a 'ChainFollower' as a 'ChainStream'. A thread is forked to run the
-- 'ChainFollower' in the background.
withStreamingFromBlockChain
    :: NetworkLayer IO (CardanoBlock StandardCrypto)
    -- ^ ChainFollower provider
    -> Tracer IO ChainFollowLog
    -- ^ ChainFollower logger
    -> NewBuffer IO
    -- ^ Cell provider
    -> ContT r IO (ChainStream (NonEmpty ConsensusBlock) x)
withStreamingFromBlockChain network tr newCell = do
    ( service
        , Callback rollForward'
            :* Callback rollBackward
            :* Callback points
            :* Nil
        ) <-
        liftIO $ newElementStream chainWitnesses newCell
    let cf =
            ChainFollower
                { checkpointPolicy = mempty
                , readChainPoints = points ()
                , rollForward = \blocks nodeTip ->
                    rollForward' (nodeTip, blocks)
                , rollBackward
                }
    _ <- ContT $ withAsync $ chainSync network tr cf
    pure service

explodeBlock :: IsEra era => Block era -> (BHeader :*: ([] :.: Tx)) era
explodeBlock block =
    let txs = getEraTransactions block
        bh = getEraBHeader block
    in  (bh :*: Comp txs)

forConsensusS
    :: ChainStream (NonEmpty ConsensusBlock) r
    -> ChainStream (ConsensusBlock) r
forConsensusS = forChainStream id

eraBlockS
    :: ChainStream (ConsensusBlock) ()
    -> ChainStream (EraValue (BHeader :*: ([] :.: Tx))) ()
eraBlockS = S.maps $ \case
    Element Forward (tip, block) f ->
        (\u -> Element Forward (tip, u) f)
            $ (\y -> let (h :*: txs) = explodeBlock y in h :*: txs)
                `applyEraFunValue` fromConsensusBlock block
    Element Backward cp f -> Element Backward cp f
    Element Points () f -> Element Points () f

eraTxS
    :: ChainStream (EraValue (ctx :*: ([] :.: Tx))) r
    -> ChainStream (EraValue (ctx :*: Tx)) r
eraTxS = forChainStream q
  where
    f :: (ctx :*: ([] :.: Tx)) era -> ([] :.: (ctx :*: Tx)) era
    f (bh :*: Comp txs) = Comp $ fmap (bh :*:) txs
    q v = sequenceEraValue (f `applyEraFunValue` v)

-- | For over a 'ChainStream'. every element is replaced by a list of elements.
forChainStream
    :: Foldable t
    => (a -> t b)
    -- ^ How to unfold each element
    -> ChainStream a r
    -> ChainStream b r
forChainStream q = fix $ \go s -> case s of
    S.Return x -> S.Return x
    S.Effect m -> S.Effect $ go <$> m
    S.Step e -> case e of
        Element Forward (tip, x) g ->
            unroll tip (toList $ q x) >> go (g ())
        Element Backward cp g ->
            S.Step $ Element Backward cp $ go . g
        Element Points () g ->
            S.Step $ Element Points () $ go . g
  where
    unroll :: ChainTip -> [a] -> ChainStream a ()
    unroll _ [] = pure ()
    unroll tip (x : xs) =
        S.Step
            $ Element Forward (tip, x) (\() -> unroll tip xs)

data CallbackLogs e
    = ForwardLog ChainTip e
    | BackwardLog ChainPoint ChainPoint
    | PointsLog [ChainPoint]
    deriving (Show, Eq)

-- | Pure scanning of a 'ChainStream'.
scanChainStream
    :: (b -> a -> b)
    -- ^ How to react to new elements
    -> ChainPoints b
    -- ^ How to react to rollbacks
    -> ChainStream a r
    -- ^ The stream to scan
    -> Stream (Of b) (Stream (Of (CallbackLogs a)) IO) r
    -- ^ The scanned stream
scanChainStream rollForward state = go state . hoist lift
  where
    go chainPoints s = case s of
        S.Return x -> S.Return x
        S.Effect m -> S.Effect $ go chainPoints <$> m
        S.Step e -> case e of
            Element Forward (tip, x) g -> do
                let acc = rollForward (current chainPoints) x
                    chainPoints' =
                        feed
                            chainPoints
                            (chainPointFromChainTip tip)
                            acc
                lift $ S.yield $ ForwardLog tip x
                S.Step $ acc :> go chainPoints' (g ())
            Element Backward cp g -> do
                let (cp', chainPoints') = rollback chainPoints cp
                lift $ S.yield $ BackwardLog cp cp'
                S.Effect $ pure $ go chainPoints' $ g cp'
            Element Points () g -> do
                let cps = points chainPoints
                lift $ S.yield $ PointsLog cps
                S.Effect $ pure $ go chainPoints (g cps)

mapMChainStream
    :: (a -> IO b)
    -> (ChainPoint -> IO ChainPoint)
    -> ChainStream a r
    -> ChainStream b r
mapMChainStream f h = S.mapped $ \case
    Element Forward (tip, x) g ->
        Element Forward . (tip,) <$> f x <*> pure g
    Element Backward cp g ->
        Element Backward <$> h cp <*> pure g
    Element Points () g -> pure $ Element Points () g

printChainStream
    :: Show a
    => ChainStream a r
    -> ChainStream a r
printChainStream =
    mapMChainStream
        (\x -> putStrLn ("element: \n" <> show x) >> pure x)
        (\x -> putStrLn ("rollback: \n" <> show x) >> pure x)
