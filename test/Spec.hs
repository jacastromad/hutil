module Main where

import Test.Hspec

import qualified GridSpec
import qualified PQueueSpec

main :: IO ()
main = hspec $ do
  GridSpec.spec
  PQueueSpec.spec

