{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Network.Rollback.One
    ( oneHistory
    )
where

import Prelude

import Cardano.Wallet.Network.Rollback.ChainPoints
    ( ChainPoints (..)
    , WithChainPoint (..)
    , chainPointDifference
    )
import Cardano.Wallet.Read
    ( ChainPoint (..)
    )
import Control.Monad.Fix
    ( fix
    )
import Numeric.Natural
    ( Natural
    )

data OneHistory b = OneHistory
    { old :: WithChainPoint b
    -- the promised state that is at least n slots behind the tip
    , candidate :: WithChainPoint b
    -- the state that is going to substitute the old one when it gets old enough
    , latest :: WithChainPoint b
    -- the most recent state
    }

-- | Create a 'ChainPoints' store with a maximum number of slots to keep in
-- history. It will keep at least one state at the given distance from the
-- tip.
oneHistory
    :: Natural
    -- ^ Maximum number of slots to keep in history
    -> b
    -- ^ Initial value
    -> ChainPoints b
oneHistory n b0 =
    ($ start)
        $ fix
        $ \go actual@OneHistory{old, candidate, latest} ->
            ChainPoints
                { rollback = \cp ->
                    if point latest <= cp
                        then (cp, go actual)
                        else
                            if point old <= cp
                                then (point old, go $ OneHistory old old old)
                                else (GenesisPoint, go start)
                , feed = \cp b ->
                    let new = Valued cp b
                    in  go
                            $ if chainPointDifference cp (point candidate) >= n
                                then OneHistory candidate new new
                                else OneHistory old candidate new
                , current = value latest
                , points = map point [old, candidate, latest]
                }
  where
    start = OneHistory v0 v0 v0
    v0 = Valued GenesisPoint b0
