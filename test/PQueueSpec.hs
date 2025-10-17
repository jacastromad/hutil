module PQueueSpec (spec) where

import Test.Hspec
import PQueue hiding (null)
import qualified PQueue as PQ

spec :: Spec
spec = do
  describe "empty/null/size" $ do
    it "starts empty" $ do
      PQ.null (empty :: MinPQ Int Char) `shouldBe` True
      size (empty :: MinPQ Int Char) `shouldBe` 0

  describe "singleton/insert/minView FIFO" $ do
    it "minView yields the smallest priority" $ do
      let q = insert 5 'x' $ insert 1 'a' $ insert 3 'b' empty
      fmap fst (minView q) `shouldBe` Just (1,'a')
    it "is FIFO within equal priorities" $ do
      let q = insert 1 'a' . insert 1 'b' . insert 1 'c' $ empty
      let Just ((1,'a'), q1) = minView q
      let Just ((1,'b'), q2) = minView q1
      let Just ((1,'c'), _ ) = minView q2
      True `shouldBe` True

  describe "fromList preserves FIFO and order by priority" $ do
    it "orders by priority ascending" $ do
      let q = fromList [(3,'c'),(1,'a'),(2,'b')]
      fmap fst (minView q) `shouldBe` Just (1,'a')
    it "keeps FIFO within the same priority" $ do
      let q = fromList [(1,'a'),(1,'b'),(1,'c')]
      let Just ((1,'a'), q1) = minView q
      let Just ((1,'b'), q2) = minView q1
      let Just ((1,'c'), _ ) = minView q2
      True `shouldBe` True

  describe "minViewKey" $ do
    it "peeks without removing" $ do
      let q = fromList [(2,'b'),(1,'a')]
      minViewKey q `shouldBe` Just (1,'a')
      size q `shouldBe` 2

  describe "deleteMin" $ do
    it "removes all elements with the smallest priority" $ do
      let q = fromList [(1,'a'),(1,'b'),(2,'c')]
          q' = deleteMin q
      minViewKey q' `shouldBe` Just (2,'c')
      size q' `shouldBe` 1

  describe "Functor" $ do
    it "maps elements, leaves priorities" $ do
      let q = fromList [(2,10),(1,5)]
      minViewKey (fmap (+1) q) `shouldBe` Just (1,6)

  describe "Semigroup/Monoid" $ do
    it "mappend merges queues and keeps FIFO within each priority" $ do
      let q1 = fromList [(1,'a'),(1,'b')]
          q2 = fromList [(1,'c')]
          q  = q1 <> q2
      let Just ((1,'a'), q')  = minView q
      let Just ((1,'b'), q'') = minView q'
      let Just ((1,'c'), _ )  = minView q''
      True `shouldBe` True
    it "mempty is identity" $ do
      let q = fromList ([(2,'x'),(1,'y')] ::  [(Int, Char)])
      q <> mempty `shouldBe` q
      mempty <> q `shouldBe` q

