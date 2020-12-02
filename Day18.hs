module Main where

import qualified Data.Map.Strict as Map
import System.IO.Unsafe
import Data.Maybe

type Regs = Map.Map Char Int

data Ins = Snd Char | Set Char Value | Add Char Value | Mul Char Value | Mod Char Value | Rcv Char | Jgz Char Value | Snd2 Value
  deriving Show
data Value = Reg Char | Val Int
  deriving Show
data State = Took | Didnt | Waiting | Put
  deriving Eq
-- (pc, regs, receiveQueue)
type Program = (Int, Regs, [Int])

main = do putStrLn (show $ day18_2 real)

day18_1 :: String -> Int
day18_1 s = doIns1 ins (head ins) 0 0 Map.empty
  where ins = processInput s

--runPrograms :: [Ins] -> Int -> [Int] -> [Int] -> Program -> Program -> Int
--runPrograms ins p queue0 queue1 (pc0, rs0, rcv0) (pc1, rs1, rcv1) 
day18_2 :: String -> Int
day18_2 s = runPrograms ins 0 (0, Map.fromList [('p',0)], []) (0, Map.fromList [('p',1)], [])
  where ins = processInput2 s

processInput :: String -> [Ins]
processInput s = map makeInstruction $ lines s

processInput2 :: String -> [Ins]
processInput2 s = map makeInstruction2 $ lines s

makeInstruction2 :: String -> Ins
makeInstruction2 s
  | ins == "snd" = Snd2 v2
  | ins == "set" = Set r v
  | ins == "add" = Add r v
  | ins == "mul" = Mul r v
  | ins == "mod" = Mod r v
  | ins == "rcv" = Rcv r
  | ins == "jgz" = Jgz r v
  where ws = words s
        ins = head ws
        r = head (ws!!1)
        v = case reads (ws!!2) of [(val,"")] -> Val val
                                  _          -> Reg (head (ws!!2))
        v2 = case reads (ws!!1) of [(val,"")] -> Val val
                                   _          -> Reg (head (ws!!1))


makeInstruction :: String -> Ins
makeInstruction s
  | ins == "snd" = Snd r
  | ins == "set" = Set r v
  | ins == "add" = Add r v
  | ins == "mul" = Mul r v
  | ins == "mod" = Mod r v
  | ins == "rcv" = Rcv r
  | ins == "jgz" = Jgz r v
  where ws = words s
        ins = head ws
        r = head (ws!!1)
        v = case reads (ws!!2) of [(val,"")] -> Val val
                                  _          -> Reg (head (ws!!2))

doIns1 :: [Ins] -> Ins -> Int -> Int -> Regs -> Int
doIns1 is (Snd r) snd pc rs = doIns1 is (is!!(pc+1)) (rs!!!!r) (pc+1) rs
doIns1 is (Set r v) snd pc rs = doIns1 is (is!!(pc+1)) snd (pc+1) (Map.insert r (getValue v rs) rs)
doIns1 is (Add r v) snd pc rs = doIns1 is (is!!(pc+1)) snd (pc+1) (Map.insert r ((getValue v rs) + (rs!!!!r)) rs)
doIns1 is (Mul r v) snd pc rs = doIns1 is (is!!(pc+1)) snd (pc+1) (Map.insert r ((getValue v rs) * (rs!!!!r)) rs)
doIns1 is (Mod r v) snd pc rs = doIns1 is (is!!(pc+1)) snd (pc+1) (Map.insert r ((rs!!!!r) `mod` (getValue v rs)) rs)
doIns1 is (Rcv r) snd pc rs = if (rs!!!!r) == 0 then doIns1 is (is!!(pc+1)) snd (pc+1) rs else snd
doIns1 is (Jgz r v) snd pc rs = if (rs!!!!r) > 0 then doIns1 is (is!!(pc+(getValue v rs))) snd (pc + (getValue v rs)) rs else doIns1 is (is!!(pc+1)) snd (pc+1) rs

runPrograms :: [Ins] -> Int -> Program -> Program -> Int
runPrograms ins p (pc0, rs0, rcv0) (pc1, rs1, rcv1) = seq pc0' $ seq pc1' $ seq rs0' $ seq rs1' $ seq rcv0' $ seq rcv1' $ seq p' $ if waiting0 && waiting1 then p else seq test $ runPrograms ins p' (pc0', rs0', rcv0') (pc1', rs1', rcv1')
  where (pc0', rs0', rcv1', state0) = doIns2 ins (ins!!pc0) pc0 rs0 rcv0
        (pc1', rs1', rcv0', state1) = doIns2 ins (ins!!pc1) pc1 rs1 rcv1
        waiting0 = state0 == Waiting
        waiting1 = state1 == Waiting
        p' = p + if state1 == Put then 1 else 0
        test = if p' `mod` 10000 == 0 then unsafePerformIO $ do putStrLn ("p = " ++ (show p')) else ()-- ++ ", q0 = " ++ show queue0'' ++ ", q1 = " ++ (show queue1''))

getFromQueue :: [Int] -> Maybe Int
getFromQueue [] = Nothing
getFromQueue is = Just (last is)

doIns2 :: [Ins] -> Ins -> Int -> Regs -> [Int] -> (Int, Regs, [Int], State)
doIns2 is (Snd2 v) pc rs recs = seq (logStr "snd") $ ((pc+1), rs, (getValue v rs):recs, Put)
doIns2 is (Set r v) pc rs recs = seq (logStr "set") $ ((pc+1), (Map.insert r (getValue v rs) rs), recs, Didnt)
doIns2 is (Add r v) pc rs recs = seq (logStr "add") $ ((pc+1), (Map.insert r ((getValue v rs) + (rs!!!!r)) rs), recs, Didnt)
doIns2 is (Mul r v) pc rs recs = seq (logStr "mul") $ ((pc+1), (Map.insert r ((getValue v rs) * (rs!!!!r)) rs), recs, Didnt)
doIns2 is (Mod r v) pc rs recs = seq (logStr "mod") $ ((pc+1), (Map.insert r ((rs!!!!r) `mod` (getValue v rs)) rs), recs, Didnt)
doIns2 is (Rcv r) pc rs [] = seq (logStr "rcv") $ (pc, rs, [], Waiting)
doIns2 is (Rcv r) pc rs recs = seq (logStr "rcv") $ ((pc+1), (Map.insert r (last recs) rs), init recs, Took)
doIns2 is (Jgz r v) pc rs recs = seq (logStr "jgz") $ if (rs!!!!r) > 0 then (pc+(getValue v rs), rs, recs, Didnt) else ((pc+1), rs, recs, Didnt)

logStr :: String -> ()
logStr s = ()--unsafePerformIO $ do putStrLn s

getValue :: Value -> Regs -> Int
getValue (Reg r) rs = rs!!!!r
getValue (Val v) _ = v

(!!!!) :: Regs -> Char -> Int
(!!!!) rs k = case Map.lookup k rs of Just i  -> i
                                      Nothing -> 0

test1 = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
test2 = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"
test3 = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nsnd 3\nsnd 5\nrcv d\nrcv e\nrcv f"
real = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 735\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"