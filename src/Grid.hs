-- Grid.hs
module Grid where

import qualified Data.Vector.Unboxed as U

data Grid a = Grid {width :: !Int, height :: !Int, buf :: !(U.Vector a)}

instance (U.Unbox a, Show a) => Show (Grid a) where
  show (Grid w h v) = "Grid: width=" ++ show w ++ " height=" ++ show h ++ "\n" ++
    unlines [unwords [show (v U.! (y*w + x)) | x <- [0..w-1]] | y <- [0..h-1]]

-- constructor
fromDims :: U.Unbox a => Int -> Int -> a -> Grid a
fromDims w h a
  | w < 0 || h < 0 = error "fromDims: negative size"
  | otherwise      = Grid w h (U.replicate (w*h) a)

-- create a grid from a list of rows of char
fromRows :: U.Unbox a => [[a]] -> Grid a
fromRows []       = error "fromRows: empty rows"
fromRows ([]:_)   = error "fromRows: empty first row"
fromRows rs@(r:_) = if all ((==w) . length) rs
                      then Grid w h (U.fromListN (w*h) $ concat rs)
                      else error "fromRows: ragged rows"
  where (w, h) = (length r, length rs)

-- bounds check for (x, y)
inBounds :: Grid a -> (Int, Int) -> Bool
inBounds (Grid w h _) (x, y) = x>=0 && y>=0 && x<w && y<h

-- linear index - row-major
lrm :: Grid a -> (Int, Int) -> Int
lrm (Grid w _ _) (x, y) = y*w + x

-- read at (x, y)
(!@) :: U.Unbox a => Grid a -> (Int,Int) -> a
(!@) g p = buf g U.! lrm g p

-- read at (x, y) (no OOB)
(!?) :: U.Unbox a => Grid a -> (Int,Int) -> Maybe a
(!?) g p = if inBounds g p then Just (g !@ p) else Nothing

infixl 9 !@
infixl 9 !?

-- positions containing a given element
findInds :: (Eq a, U.Unbox a) => a -> Grid a -> [(Int, Int)]
findInds e g = [ (i `mod` width g, i `div` width g)
               | i <- U.toList (U.findIndices (== e) (buf g))]

-- inbound left, right, up and down positions
neighbors4 :: Grid a -> (Int, Int) -> [(Int, Int)]
neighbors4 g (x, y) = filter (inBounds g) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- inbound surrounding positions
neighbors8 :: Grid a -> (Int, Int) -> [(Int, Int)]
neighbors8 g (x, y) = filter (inBounds g) ps
  where ps = [(x+dx, y+dy) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], (dx, dy) /= (0, 0)]


