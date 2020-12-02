module Main where

import Data.Int

aSeed = 679
bSeed = 771

main = do putStrLn $ "Part 1: " ++ (show day15_1) 
          putStrLn $ "Part 2: " ++ (show day15_2)

day15_1 :: Int
day15_1 = generateAndCompare aSeed bSeed generatePair 40000000 0

day15_2 :: Int
day15_2 = generateAndCompare aSeed bSeed generatePair' 5000000 0

generateAndCompare :: Int -> Int -> (Int -> Int -> (Int, Int))-> Int ->  Int -> Int
generateAndCompare a b f 0 n = n
generateAndCompare a b f i n = generateAndCompare a' b' f (i-1) n'
  where (a',b') = f a b
        n' = if low16Equals a' b' then n+1 else n

low16Equals :: Int -> Int -> Bool
low16Equals a b = a' == b'
  where a' = (fromIntegral a) :: Int16
        b' = (fromIntegral b) :: Int16

generatePair :: Int -> Int -> (Int, Int)
generatePair a b = (generateNewValue a 16807, generateNewValue b 48271)

generatePair' :: Int -> Int -> (Int, Int)
generatePair' a b = (generateNewValue' a 16807 4, generateNewValue' b 48271 8)

generateNewValue :: Int -> Int -> Int
generateNewValue i x = (i * x) `mod` 2147483647

generateNewValue' :: Int -> Int -> Int -> Int
generateNewValue' i x y = if i' `mod` y == 0 then i' else generateNewValue' i' x y
  where i' = (i * x) `mod` 2147483647