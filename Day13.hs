module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe

data Layer = Wall Int Int Dir
  deriving Show
data Dir = Up | Down
  deriving Show

type LMap = Map.Map Int Layer

main :: IO ()
main = do print $ day13_2 real

day13_1 :: String -> Int
day13_1 s = sum $ snd $ makeStep 1 0 max m []
  where m = processInput s
        max = maximum $ Map.keys m

day13_2 :: String -> Int
day13_2 s = delay 10 max m
  where m = processInput s
        max = maximum $ Map.keys m

delay :: Int -> Int -> LMap -> Int
delay d max m = if is == [] then d else (delay (d+1) max m')
  where m' = iterateStepMap m 1
        (_,is) = makeStep 0 0 max m []

makeStep :: Int -> Int -> Int -> LMap -> [Int] -> (LMap, [Int]) 
makeStep 0 step max m caughts
  | step > max = (m, caughts)
  | otherwise  = makeStep 0 (step+1) max m' caughts'
  where caught = case getScannerPos step m of Nothing -> False
                                              Just a  -> a == 0
        m' = stepMap m
        layerRange = getLayerRange step m
        severity = layerRange*step
        caughts' = if caught then severity:caughts else caughts
makeStep delay step max m caughts = seq 5 $ makeStep 0 step max (iterateStepMap m delay) caughts 

getScannerPos :: Int -> LMap -> Maybe Int
getScannerPos i m = case Map.lookup i m of Nothing           -> Nothing
                                           Just (Wall _ p _) -> Just p

getLayerRange :: Int -> LMap -> Int
getLayerRange i m = (\(Wall i _ _) -> i) (fromJust $ Map.lookup i m)

processInput :: String -> LMap
processInput s = makeMap iss Map.empty
  where lss = map words $ lines s
        iss = map (\(a:b:_) -> (read a,read b)) lss

makeMap :: [(Int,Int)] -> LMap -> LMap
makeMap [] m = m
makeMap ((k,v):xs) m = makeMap xs (Map.insert k (Wall v 0 Down) m)

iterateStepMap :: LMap -> Int -> LMap
iterateStepMap m 0 = m
iterateStepMap m n = iterateStepMap (stepMap m) (n-1)

stepMap :: LMap -> LMap
stepMap m = Map.map stepLayer m

stepLayer :: Layer -> Layer
stepLayer (Wall s p Up)
  | p == 0    = (Wall s (p+1) Down)
  | otherwise = (Wall s (p-1) Up)
stepLayer (Wall s p Down)
  | p == (s-1) = (Wall s (p-1) Up)
  | otherwise  = (Wall s (p+1) Down)

test  = "0 3\n1 2\n4 4\n6 4"
real  = "0 3\n1 2\n2 6\n4 4\n6 4\n8 8\n10 6\n12 8\n14 5\n16 6\n18 8\n20 8\n22 12\n24 6\n26 9\n28 8\n30 12\n32 12\n34 17\n36 12\n38 8\n40 12\n42 12\n44 10\n46 12\n48 12\n50 12\n52 14\n54 14\n56 10\n58 14\n60 12\n62 14\n64 14\n66 14\n68 14\n70 14\n72 14\n74 14\n76 14\n86 14\n94 20\n96 18"
