module SearchSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import Search

spec :: Spec
spec = do
  describe "buildPath" $ do
    it "reconstructs an ancestor chain" $ do
      let parents = M.fromList [('b','a'), ('c','b'), ('d','c')]
      buildPath parents 'd' `shouldBe` ['a','b','c','d']

    it "returns just the start when no parent exists" $ do
      buildPath M.empty 'x' `shouldBe` ['x']

  describe "dfsPath / bfsPath on small cyclic graph" $ do
    -- Undirected-like adjacency via symmetric edges
    let adj =
          M.fromList
            [ ('A',['B','D'])
            , ('B',['A','C','E'])
            , ('C',['B','F'])
            , ('D',['A','E'])
            , ('E',['B','D','G'])
            , ('F',['C'])
            , ('G',['E'])
            ]
        neigh v = M.findWithDefault [] v adj
        goal x = x == 'F'
    it "returns Just [start] when start is goal (DFS)" $
      dfsPath 'F' neigh goal `shouldBe` Just ['F']
    it "returns Just [start] when start is goal (BFS)" $
      bfsPath 'F' neigh goal `shouldBe` Just ['F']
    it "finds a valid path to F (DFS)" $
      dfsPath 'A' neigh goal `shouldBe` Just ['A','B','C','F']
    it "finds the shortest path to F (BFS)" $
      bfsPath 'A' neigh goal `shouldBe` Just ['A','B','C','F']

  describe "unreachable goal" $ do
    let adj = M.fromList [('a',['b']), ('b',[])]  -- 'z' is disconnected
        neigh v = M.findWithDefault [] v adj
        goal x = x == 'z'
    it "returns Nothing with DFS" $
      dfsPath 'a' neigh goal `shouldBe` Nothing
    it "returns Nothing with BFS" $
      bfsPath 'a' neigh goal `shouldBe` Nothing

  describe "BFS gives shortest path, DFS may not" $ do
    let adj = M.fromList
                [('A',['B','S'])
                ,('B',['C'])
                ,('C',['D'])
                ,('D',['G'])
                ,('S',['G'])
                ,('G',[])]
        neigh v = M.findWithDefault [] v adj
        isG x = x == 'G'
    it "DFS returns the first found deep path" $
      dfsPath 'A' neigh isG `shouldBe` Just ['A','B','C','D','G']
    it "BFS returns the shortest path" $
      bfsPath 'A' neigh isG `shouldBe` Just ['A','S','G']

