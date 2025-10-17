-- PQueue.hs
module PQueue where

import Prelude hiding (null)
import qualified Prelude as P
import qualified Data.Map.Strict as M
import Data.List (foldl')

-- min-priority queue on top of Map
newtype MinPQ p a = MinPQ (M.Map p [a])  -- smallest p first
  deriving (Show, Eq)

instance Functor (MinPQ p) where
  fmap f (MinPQ m) = MinPQ (fmap (map f) m)

instance Ord p => Semigroup (MinPQ p a) where
  MinPQ a <> MinPQ b = MinPQ (M.unionWith (++) a b)

instance Ord p => Monoid (MinPQ p a) where
  mempty = empty

-- empty min-priority queue
empty :: MinPQ p a
empty = MinPQ M.empty
{-# INLINE empty #-}

-- min-priority queue with one element
singleton :: p -> a -> MinPQ p a
singleton p a = MinPQ $ M.singleton p [a]
{-# INLINE singleton #-}

-- insert element a with priority p
insert :: Ord p => p -> a -> MinPQ p a -> MinPQ p a
insert k x (MinPQ m) = MinPQ (M.insertWith (++) k [x] m)
{-# INLINE insert #-}

-- min-priority queue from list of pairs (priority, element)
fromList :: Ord p => [(p, a)] -> MinPQ p a
fromList = foldl' (\q (p,a) -> insert p a q) empty

-- True if empty
null :: MinPQ p a -> Bool
null (MinPQ m) = M.null m
{-# INLINE null #-}

-- size of the elements (not the priorities/keys)
size :: MinPQ p a -> Int
size (MinPQ m) = sum (map length (M.elems m))

-- return smallest priority element and the rest of the min-priority queue
minView :: Ord p => MinPQ p a -> Maybe ((p, a), MinPQ p a)
minView (MinPQ m) = case M.minViewWithKey m of
  Nothing              -> Nothing
  Just ((k, y:ys), m') -> Just ((k, y), MinPQ (if P.null ys then m' else M.insert k ys m'))
  Just ((_, []), _)    -> error "minView: empty list of elements"  -- unreachable

-- peek the smallest priority element
minViewKey :: Ord p => MinPQ p a -> Maybe (p, a)
minViewKey (MinPQ m) = do
  (k, xs) <- M.lookupMin m
  case xs of { y:_ -> Just (k, y); [] -> Nothing }  -- [] is unreachable

-- remove all min priority elements
deleteMin :: Ord p => MinPQ p a -> MinPQ p a
deleteMin (MinPQ m) = MinPQ (maybe m snd (M.minView m))


