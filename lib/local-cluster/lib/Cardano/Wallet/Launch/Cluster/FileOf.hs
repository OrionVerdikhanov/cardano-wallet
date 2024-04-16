{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf
    , AbsFileOf
    , DirOf
    , AbsDirOf
    , RelDirOf
    , changeFileOf
    , changeDirOf
    , mkAbsolutize
    , Absolutize (..)
    , absolutize
    )
where

import Prelude

import GHC.TypeLits
    ( Symbol
    )
import Path
    ( Abs
    , Dir
    , File
    , Path
    , Rel
    , SomeBase (..)
    , (</>)
    )
import Path.IO
    ( getCurrentDir
    )

type FileOf (s :: Symbol) t = Path t File

type AbsFileOf (s :: Symbol) = Path Abs File

type DirOf (s :: Symbol) t = Path t Dir

type AbsDirOf (s :: Symbol) = Path Abs Dir

type RelDirOf (s :: Symbol) = Path Rel Dir

changeFileOf :: forall a b t. FileOf a t -> FileOf b t
changeFileOf fp = fp

changeDirOf :: DirOf a t -> DirOf b t
changeDirOf dp = dp

newtype Absolutize = Absolutize (forall t. SomeBase t -> Path Abs t)

mkAbsolutize :: IO Absolutize
mkAbsolutize = do
    cwd <- getCurrentDir
    pure $ Absolutize $ \case
        Abs p -> p
        Rel p -> cwd </> p

absolutize :: SomeBase t -> IO (Path Abs t)
absolutize x = do
    Absolutize f <- mkAbsolutize
    pure $ f x
