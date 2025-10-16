-- Search.hs
module Search where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import Data.Foldable (foldl')
import Data.List (unfoldr)


-- helper to build a path walking the map backwards
buildPath :: Ord a => M.Map a a -> a -> [a]
buildPath m = reverse . unfoldr (\s -> s >>= \cur -> Just (cur, M.lookup cur m)) . Just


-- generic DFS with goal and path reconstruction.
dfsPath :: Ord a => a            -- starting node
                 -> (a -> [a])   -- neighbors
                 -> (a -> Bool)  -- is goal
                 -> Maybe [a]    -- the path (maybe)
dfsPath start neigh goal = if goal start
                             then Just [start]
                             else go (S.singleton start) [start] M.empty
  where
    go _ [] _ = Nothing
    go seen (p:stk) parent
      | goal p    = Just (buildPath parent p)
      | otherwise = go seen' stk' parent'
      where
        fresh   = filter (`S.notMember` seen) (neigh p)
        seen'   = foldl' (flip S.insert) seen fresh
        parent' = foldl' (\m q -> M.insert q p m) parent fresh
        stk'    = foldr (:) stk fresh


-- generic BFS with goal and path reconstruction.
bfsPath :: Ord a => a            -- starting node
                 -> (a -> [a])   -- neighbors
                 -> (a -> Bool)  -- is goal
                 -> Maybe [a]    -- the path (maybe)
bfsPath start neigh goal = if goal start
                             then Just [start]
                             else go (S.singleton start) (Q.singleton start) M.empty
  where
    go _ Q.Empty _ = Nothing
    go seen (p Q.:<| qs) parent
      | goal p    = Just (buildPath parent p)
      | otherwise = go seen' qs' parent'
      where
        fresh   = filter (`S.notMember` seen) (neigh p)
        seen'   = foldl' (flip S.insert) seen fresh
        parent' = foldl' (\m q -> M.insert q p m) parent fresh
        qs'     = qs Q.>< Q.fromList fresh


