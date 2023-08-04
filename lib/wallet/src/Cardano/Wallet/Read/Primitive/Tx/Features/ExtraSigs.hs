{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Features.ExtraSigs (extraSigs) where

import Cardano.Crypto.Hash
  ( Hash (..)
  )
import Cardano.Ledger.Crypto
  ( StandardCrypto
  )
import Cardano.Ledger.Keys
  ( KeyHash (..)
  , KeyRole (..)
  )
import Cardano.Wallet.Primitive.Types.Hash qualified as W
import Cardano.Wallet.Read.Eras
  ( EraFun (..)
  , K (..)
  )
import Cardano.Wallet.Read.Tx.ExtraSigs
  ( ExtraSigs (..)
  )
import Data.ByteString.Short
  ( fromShort
  )
import Data.Foldable
  ( toList
  )
import Data.Functor
  ( (<&>)
  )
import Data.Set
  ( Set
  )
import Prelude

extraSigs :: EraFun ExtraSigs (K [W.Hash "ExtraSignature"])
extraSigs =
  EraFun
    { byronFun = noExtraSigs
    , shelleyFun = noExtraSigs
    , allegraFun = noExtraSigs
    , maryFun = noExtraSigs
    , alonzoFun = yesExtraSigs
    , babbageFun = yesExtraSigs
    , conwayFun = yesExtraSigs
    }
  where
    noExtraSigs = const $ K []
    yesExtraSigs (ExtraSigs es) = K $ getExtraSigs es

getExtraSigs
  :: Set (KeyHash 'Witness StandardCrypto)
  -> [W.Hash "ExtraSignature"]
getExtraSigs es =
  toList es <&> \(KeyHash (UnsafeHash h)) -> W.Hash $ fromShort h
