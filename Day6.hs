module Day6 where

import Data.List
import Data.Maybe

day6_1 :: String -> Int
day6_1 s = distributeLoop (processInput s) [] 0

processInput :: String -> [Int]
processInput s = map read (words s)

distributeLoop :: [Int] -> [[Int]] -> Int -> Int
distributeLoop is seen n 
  | is `elem` seen = n
  | otherwise      = distributeLoop is' (is:seen) (n+1)
  where
    b   = maximum is
    p   = fromJust (elemIndex b is)
    is' = distribute (insertIntoList is p 0) b (p+1)

insertIntoList is l v = (take l is) ++ [v] ++ (drop (l+1) is)

distribute :: [Int] -> Int -> Int -> [Int]
distribute is 0 _ = is
distribute is n p = distribute newList (n-1) (p+1)
  where l       = (p `mod` (length is))
        newList = insertIntoList is l ((is!!l)+1)

test1 = "0 2 7 0"
real1 = "14 0 15  12  11  11  3 5 1 6 8 4 9 1 8 4"

day6_2 :: String -> Int
day6_2 s = distributeLoop' (processInput s) [] 0

distributeLoop' :: [Int] -> [([Int], Int)] -> Int -> Int
distributeLoop' is seen n 
  | isJust (lookup is seen) = n-(fromJust (lookup is seen))
  | otherwise      = distributeLoop' is' ((is, n):seen) (n+1)
  where
    b   = maximum is
    p   = fromJust (elemIndex b is)
    is' = distribute (insertIntoList is p 0) b (p+1)