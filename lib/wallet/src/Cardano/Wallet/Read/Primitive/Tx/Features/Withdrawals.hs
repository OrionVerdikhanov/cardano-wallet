{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
  ( getWithdrawals
  , fromLedgerWithdrawals
  )
where

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin
  )
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import Cardano.Wallet.Primitive.Types.RewardAccount
  ( RewardAccount
  )
import Cardano.Wallet.Read.Eras
  ( EraFun (..)
  , K (..)
  )
import qualified Cardano.Wallet.Read.Primitive.Tx.Features.Certificates as Certificates
import Cardano.Wallet.Read.Tx.Withdrawals
  ( Withdrawals (..)
  )
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Ledger
import qualified Data.Map as Map
import Data.Map.Strict
  ( Map
  )
import Prelude

getWithdrawals :: EraFun Withdrawals (K (Maybe (Map RewardAccount Coin)))
getWithdrawals =
  EraFun
    { byronFun = \_withdrawals -> K Nothing
    , shelleyFun = eraFromWithdrawals
    , allegraFun = eraFromWithdrawals
    , maryFun = eraFromWithdrawals
    , alonzoFun = eraFromWithdrawals
    , babbageFun = eraFromWithdrawals
    , conwayFun = eraFromWithdrawals
    }
  where
    eraFromWithdrawals (Withdrawals withdrawals) =
      K . Just $ fromLedgerWithdrawals withdrawals

fromLedgerWithdrawals
  :: (Map (Ledger.RewardAcnt crypto) Ledger.Coin) -> Map RewardAccount W.Coin
fromLedgerWithdrawals withdrawals =
  Map.fromList
    [ (Certificates.fromStakeCredential cred, Ledger.toWalletCoin coin)
    | (Ledger.RewardAcnt _network cred, coin) <- Map.toList withdrawals
    ]
