module GridSpec (spec) where

import Test.Hspec
import Grid
import Control.Exception (evaluate)
--import qualified Data.Vector.Unboxed as U


spec :: Spec
spec = do
  describe "fromDims" $ do
    it "creates w*h elements filled with a" $ do
      let g = fromDims 3 2 (7 :: Int)
      width g  `shouldBe` 3
      height g `shouldBe` 2
      map (g !@) [(0,0),(2,0),(0,1),(2,1)] `shouldBe` [7,7,7,7]
    it "rejects negative sizes" $ do
      evaluate (fromDims (-1) 2 (0::Int)) `shouldThrow` errorCall "fromDims: negative size"
      evaluate (fromDims  1 (-2) (0::Int)) `shouldThrow` errorCall "fromDims: negative size"

  describe "fromRows" $ do
    it "builds a 2x2 char grid" $ do
      let g = fromRows ["ab","cd"]
      width g  `shouldBe` 2
      height g `shouldBe` 2
      [g !@ (0,0), g !@ (1,0), g !@ (0,1), g !@ (1,1)] `shouldBe` "abcd"
    it "errors on empty outer list" $
      evaluate (fromRows [] :: Grid Char) `shouldThrow` errorCall "fromRows: empty rows"
    it "errors on empty first row" $
      evaluate (fromRows [[],[]] :: Grid Char) `shouldThrow` errorCall "fromRows: empty first row"
    it "errors on ragged rows" $
      evaluate (fromRows ["ab","c"] :: Grid Char) `shouldThrow` errorCall "fromRows: ragged rows"

  describe "inBounds / lrm / indexing" $ do
    let g = fromRows ["abc","def"]  -- 3x2
    it "inBounds detects valid and invalid coords" $ do
      inBounds g (0,0) `shouldBe` True
      inBounds g (2,1) `shouldBe` True
      inBounds g (3,0) `shouldBe` False
      inBounds g (-1,0) `shouldBe` False
      inBounds g (0,2) `shouldBe` False
    it "lrm is row-major" $ do
      lrm g (0,0) `shouldBe` 0
      lrm g (2,0) `shouldBe` 2
      lrm g (0,1) `shouldBe` 3
      lrm g (2,1) `shouldBe` 5
    it "(!@) and (!?) read cells" $ do
      g !@ (1,0) `shouldBe` 'b'
      g !? (1,0) `shouldBe` Just 'b'
      g !? (5,5) `shouldBe` Nothing

  describe "findInds" $ do
    let g = fromRows ["aba","cba"] -- positions of 'a': (0,0),(2,0),(2,1)
    it "returns coordinates of matching elements in index order" $ do
      findInds 'a' g `shouldBe` [(0,0),(2,0),(2,1)]
      findInds 'z' g `shouldBe` []

  describe "neighbors4 / neighbors8" $ do
    let g = fromRows ["abc","def","ghi"] -- 3x3
    it "neighbors4 respects bounds and order" $ do
      neighbors4 g (0,0) `shouldBe` [(1,0),(0,1)]            -- right, down
      neighbors4 g (1,1) `shouldBe` [(0,1),(2,1),(1,0),(1,2)]
    it "neighbors8 returns all surrounding in-bounds cells (row-major)" $ do
      neighbors8 g (0,0) `shouldBe` [(1,0),(0,1),(1,1)]
      neighbors8 g (1,1) `shouldBe`
        [ (0,0),(1,0),(2,0)
        , (0,1)      ,(2,1)
        , (0,2),(1,2),(2,2)]

  describe "Show" $ do
    it "formats header and rows" $ do
      let g = fromRows [[1,2],[3,4] :: [Int]]
      show g `shouldBe` "Grid: width=2 height=2\n1 2\n3 4\n"

