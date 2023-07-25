module Main where

import Main.Utf8
  ( withUtf8
  )
import qualified Network.Wai.Middleware.LoggingSpec as LoggingSpec
import Test.Hspec.Extra
  ( hspecMain
  )
import Prelude

main :: IO ()
main = withUtf8 $ hspecMain LoggingSpec.spec
