-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Write.Tx
    (
    -- * Balancing transactions
      balanceTransaction
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputAdaQuantityInsufficientError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxOutputSizeExceedsLimitError (..)
    , ErrBalanceTxOutputTokenQuantityExceedsLimitError (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , ErrUpdateSealedTx (..)

    -- * Change address generation
    , ChangeAddressGen (..)

    -- * Partial transactions
    , PartialTx (..)

    -- * UTxO-related types and functions
    , UTxO
    , UTxOAssumptions (..)
    , UTxOIndex
    , constructUTxOIndex
    ) where

import Internal.Cardano.Write.Tx
    ( UTxO
    )
import Internal.Cardano.Write.Tx.Balance
    ( ChangeAddressGen (..)
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputAdaQuantityInsufficientError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxOutputSizeExceedsLimitError (..)
    , ErrBalanceTxOutputTokenQuantityExceedsLimitError (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , ErrUpdateSealedTx (..)
    , PartialTx (..)
    , UTxOAssumptions (..)
    , UTxOIndex
    , balanceTransaction
    , constructUTxOIndex
    )
