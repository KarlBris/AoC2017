module Day1 where

import Data.Char

-----------------------------------

day1_1 :: String -> String
day1_1 s = show (sumNext (makeIntList s))

makeIntList :: [Char] -> [Int]
makeIntList s = is ++ [head is]
  where is = map digitToInt s

sumNext :: [Int] -> Int
sumNext [] = 0
sumNext [_] = 0
sumNext (i:(j:xs)) = if (i==j) then (i + next) else next 
  where next = sumNext (j:xs)

-------------------------------------------------------------

day1_2 :: String -> String
day1_2 s = show (sumHalfway (makeIntListPair s))

makeIntListPair :: String -> [(Int,Int)]
makeIntListPair s = zip is ((drop s2 is) ++ (take s2 is))
  where is = map digitToInt s
        s2 = (length s) `div` 2

sumHalfway :: [(Int,Int)] -> Int
sumHalfway [] = 0
sumHalfway ((x,y):is) = if (x==y) then (x + next) else next
  where next = sumHalfway is