module Internal.Cardano.Write.Tx.Balance.CoinSelection.Gen
    ( coarbitraryWalletUTxO
    , genWalletUTxO
    , genWalletUTxOFunction
    , genWalletUTxOLargeRange
    , shrinkWalletUTxO
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn
    , genTxInLargeRange
    , shrinkTxIn
    )
import Generics.SOP
    ( NP (..)
    )
import Internal.Cardano.Write.Tx.Balance.CoinSelection
    ( WalletUTxO (..)
    )
import Test.QuickCheck
    ( Gen
    , coarbitrary
    )
import Test.QuickCheck.Extra
    ( genFunction
    , genSized2
    , genericRoundRobinShrink
    , (<:>)
    , (<@>)
    )

import qualified Cardano.Wallet.Primitive.Types.Address.Gen as W

--------------------------------------------------------------------------------
-- Wallet UTxO identifiers chosen according to the size parameter
--------------------------------------------------------------------------------

coarbitraryWalletUTxO :: WalletUTxO -> Gen a -> Gen a
coarbitraryWalletUTxO = coarbitrary . show

genWalletUTxO :: Gen WalletUTxO
genWalletUTxO = uncurry WalletUTxO <$> genSized2 genTxIn W.genAddress

shrinkWalletUTxO :: WalletUTxO -> [WalletUTxO]
shrinkWalletUTxO = genericRoundRobinShrink
    <@> shrinkTxIn
    <:> W.shrinkAddress
    <:> Nil

genWalletUTxOFunction :: Gen a -> Gen (WalletUTxO -> a)
genWalletUTxOFunction = genFunction coarbitraryWalletUTxO

--------------------------------------------------------------------------------
-- Wallet UTxO identifiers chosen from a large range
--------------------------------------------------------------------------------

genWalletUTxOLargeRange :: Gen WalletUTxO
genWalletUTxOLargeRange = WalletUTxO <$> genTxInLargeRange <*> W.genAddress
