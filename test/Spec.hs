module Main where

import Test.Hspec
import RailFenceSpec(rfSpec)

main :: IO ()
main = do
  hspec rfSpec