module Day3 where

import qualified Data.Map.Strict as Map
import Data.Maybe

data Dir = U | D | L | R 
  deriving Show

type Pos = (Int, Int)


day3_1 :: Int -> Int
day3_1 n = mDist (spiralWalk n spiral (0,0))

mDist :: (Int, Int) -> Int
mDist (a, b) = (abs a) + (abs b)

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

spiral :: [Dir]
spiral = concat (merge rl ud)
  where u = [replicate n U | n <- [1,3..]] 
        d = [replicate n D | n <- [2,4..]]
        r = [replicate n R | n <- [1,3..]]
        l = [replicate n L | n <- [2,4..]]
        ud = merge u d
        rl = merge r l

-- steps
spiralWalk :: Int -> [Dir] -> (Int, Int) -> (Int, Int)
spiralWalk 1 _ p = p
spiralWalk x (d:ds) p = spiralWalk (x-1) ds (walk d p)

walk :: Dir -> (Int, Int) -> (Int, Int)
walk U (a,b) = (a+1, b)
walk D (a,b) = (a-1, b)
walk L (a,b) = (a, b-1)
walk R (a,b) = (a, b+1)

day3_2 :: Int -> Int
day3_2 n = walkAndConstructSpiral n spiral (0,0) (Map.insert (0,0) 1 Map.empty)

walkAndConstructSpiral :: Int -> [Dir] -> Pos -> Map.Map Pos Int -> Int
walkAndConstructSpiral x (d:ds) p m 
  | v > x = v
  | otherwise = walkAndConstructSpiral x ds p' m'
  where 
    p' = walk d p
    v  = sumNeighbors p m
    m' = Map.insert p v m

sumNeighbors :: Pos -> Map.Map Pos Int -> Int
sumNeighbors (a, b) m = sum [fromJust (Map.lookup (a',b') m) | a' <- [a-1, a, a+1], b' <- [b-1, b, b+1], Map.member (a',b') m]