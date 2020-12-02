module Day10 where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Char
import Data.Bits
import Data.List.Split
import Numeric

type VMap = Map.Map Int Int

day10_1 :: String -> [Int] -> Int
day10_1 s hs = (fromJust (Map.lookup 0 res)) * (fromJust (Map.lookup 1 res))
  where res = (\(a,_,_) -> a) (tieKnots (processInput s) (populateMap hs) 0 0)

populateMap :: [Int] -> VMap
populateMap hs = Map.fromList $ zip hs hs

processInput :: String -> [Int]
processInput s = map read $ splitOn "," s

tieKnots :: [Int] -> VMap -> Int -> Int -> (VMap, Int, Int)
tieKnots [] m p s = (m, p, s)
tieKnots (i:is) m p s = tieKnots is m' p' (s+1)
  where (m', p') = tieKnot i m p s

tieKnot :: Int -> VMap -> Int -> Int -> (VMap, Int)
tieKnot i m p s = ((insertSublist p (getRevSublist m p i) m), p+i+s)

getRevSublist :: VMap -> Int -> Int -> [Int]
getRevSublist m p l = getSublist p (p+l) [] m

getSublist :: Int -> Int -> [Int] -> VMap -> [Int]
getSublist i t ls m 
  | i < t = getSublist (i+1) t ((mapGet i' m):ls) m
  | otherwise = ls
  where i' = i `mod` (Map.size m)

insertSublist :: Int -> [Int] -> VMap -> VMap
insertSublist _ [] m = m
insertSublist i (v:vs) m = insertSublist (i+1) vs (Map.insert i' v m)
  where i' = i `mod` (Map.size m)

mapGet :: Int -> VMap -> Int
mapGet k m = case Map.lookup k m of Just v  -> v
                                    Nothing -> undefined

day10_2 :: String -> [Int] -> String
day10_2 s hs = concat (map makeHex dh)
  where bs = processInput2 s
        sh = makeSparseHash 64 bs (populateMap hs) 0 0
        dh = makeDenseHash (Map.elems sh)

makeSparseHash :: Int -> [Int] -> VMap -> Int -> Int -> VMap
makeSparseHash 0 _ m _ _ = m
makeSparseHash n is m p s = makeSparseHash (n-1) is m' p' s'
  where (m', p', s') = tieKnots is m p s

makeDenseHash :: [Int] -> [Int]
makeDenseHash [] = []
makeDenseHash is = foldl xor 0 (take 16 is) : (makeDenseHash $ drop 16 is)

processInput2 :: String -> [Int]
processInput2 s = (map ord s) ++ [17, 31, 73, 47, 23]

makeHex :: Int -> String
makeHex i = if length h < 2 then ('0':h) else h
  where h = (showHex i) ""

test = "3 4 1 5"
real = "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244"