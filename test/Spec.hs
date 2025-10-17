module Main where

import Test.Hspec

import qualified GridSpec
import qualified PQueueSpec
import qualified SearchSpec

main :: IO ()
main = hspec $ do
  GridSpec.spec
  PQueueSpec.spec
  SearchSpec.spec

