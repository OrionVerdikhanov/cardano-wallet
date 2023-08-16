module Main where

import Cardano.Wallet.Spec
    ( walletSpec )
import Main.Utf8
    ( withUtf8 )
import Test.Syd
    ( sydTest )

main :: IO ()
main = withUtf8 do
    sydTest walletSpec
