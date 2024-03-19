{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0

module Cardano.Wallet.Api.Lib.ExtendedObject
    ( parseExtendedAesonObject
    , extendAesonObject
    )
    where

import Prelude

import Cardano.Wallet.Api.Lib.Options
    ( defaultRecordTypeOptions
    )
import Data.Aeson.Types
    ( Parser
    , Value (..)
    , genericParseJSON
    , genericToJSON
    , object
    , withObject
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic (..)
    )

import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson

parseExtendedAesonObject
    :: ( Generic a
       , Aeson.GFromJSON Aeson.Zero (Rep a) )
    => String
    -> Text
    -> Value
    -> Parser a
parseExtendedAesonObject txt fieldtoremove = withObject txt $ \o -> do
    let removeCertType numKey _ = Aeson.toText numKey /= fieldtoremove
    let o' = Aeson.filterWithKey removeCertType o
    genericParseJSON defaultRecordTypeOptions (Object o')

extendAesonObject
    :: ( Generic a
       , Aeson.GToJSON' Value Aeson.Zero (Rep a))
    => [Aeson.Pair]
    -> a
    -> Value
extendAesonObject tobeadded apipool =
    case (genericToJSON defaultRecordTypeOptions apipool, object tobeadded) of
        (Object obj, Object obj') -> Object (obj <> obj')
        _ -> error "extendAesonObject: impossible"
