module Test where

f :: Show a => a -> a -> String
f a b = (show a) ++ (show b)

multiply :: Int -> Int -> Int
multiply a b = a * b