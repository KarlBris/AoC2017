module Day14 where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import Day10 (day10_2)

type RMap = Map.Map (Int,Int) Char
type Pos = (Int,Int)
type Region = [Pos]

day14_1 :: String -> Int
day14_1 s = length (filter (=='1') (concat (processInput s)))

processInput :: String -> [String]
processInput s = [concat $ map hexToBin (day10_2 (s ++ "-" ++ (show x)) [0..255]) | x <- [0..127]]

day14_2 :: String -> Int
day14_2 s = fun 0 (makeMap $ processInput s)

fun :: Int -> RMap -> Int
fun i m = case regionStart of Just p -> fun (i+1) (removeRegion (makeRegion [p] m) m)
                              Nothing -> i
  where regionStart = findRegionStart m

removeRegion :: Region -> RMap -> RMap
removeRegion [] m = m
removeRegion (p:ps) m = removeRegion ps (Map.delete p m)

makeRegion :: [Pos] -> RMap -> Region
makeRegion ps m = if (length ps) == (length ps') then ps' else makeRegion ps' m
  where ps' = expandRegion ps m

expandRegion :: Region -> RMap -> Region
expandRegion ps m = ps ++ [p | p <- neighborsList ps, not (p `elem` ps), isJust (Map.lookup p m)]

findRegionStart :: RMap -> Maybe Pos
findRegionStart m = if keys /= [] then Just (head keys) else Nothing
  where keys = Map.keys m

makeMap :: [String] -> RMap
makeMap ss = Map.fromList [((x,y), ((ss!!x)!!y)) | x <- [0..127], y <- [0..127], ((ss!!x)!!y) == '1']

neighborsList :: Region -> [Pos]
neighborsList ps = nub $ concat $ map neighbors ps

neighbors :: Pos -> [Pos]
neighbors (a,b) = [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'a' = "1010"
hexToBin 'b' = "1011"
hexToBin 'c' = "1100"
hexToBin 'd' = "1101"
hexToBin 'e' = "1110"
hexToBin 'f' = "1111"