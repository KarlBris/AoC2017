module Day17 where

import qualified Data.Sequence as Seq
import System.IO.Unsafe
import Data.Maybe

type Buffer = Seq.Seq Int

day17_1 :: Int -> Int
day17_1 n = Seq.index b ((p+1) `mod` (Seq.length b))
  where b = bufferInsert 1 0 n (Seq.fromList [0]) 
        p = (fromJust $ Seq.elemIndexL 2017 b)

day17_2 :: Int -> Int
day17_2 n = bufferInsert2 1 0 n 0 

bufferInsert 2018 _ _ b = b
bufferInsert i p n b = bufferInsert (i+1) p' n b''
  where bl = Seq.length b
        p' = ((p + n) `mod` bl) + 1
        b' = id $! Seq.reverse b
        b'' = id $! Seq.reverse (Seq.insertAt (bl - p') i b')

bufferInsert2 50000000 _ _ zpo = zpo
bufferInsert2 i p n zpo = seq test $ seq p' $ seq i' $ seq zpo' (bufferInsert2 i' p' n zpo')
  where i' = (i+1)
        p' = ((p + n +1) `mod` i)
        zpo' = if p' == 0 then i else zpo

test = 3  :: Int
real = 370 :: Int