module Main where

import Test.Hspec

import qualified GridSpec

main :: IO ()
main = hspec $ do
  GridSpec.spec
