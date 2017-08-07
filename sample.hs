module Example1 where
import Rexp
import qualified Data.Map as M
import RexpInterp


{- Example DFA's -}

exampleStr1 :: String
exampleStr1 = unlines ["dfa example1 = {"
              , "alphabet = 10"
              , "states = {initial = q0; final = (q0); all = (q0, q1, q2)}"
              , "delta = {"
              , "q0 # 0 = q0;"
              , "q0 # 1 = q1;"
              , "q1 # 0 = q2;"
              , "q1 # 1 = q0;"
              , "q2 # 0 = q1;"
              , "q2 # 1 = q2"
              , "}"
              , "}"]

exampleStr2 :: String
exampleStr2 = unlines ["dfa example2 = {"
              , "alphabet = ab"
              , "states = {initial = q0; final = (q1); all = (q0, q1, q2)}"
              , "delta = {"
              , "q0 # a = q1;"
              , "q0 # b = q0;"
              , "q1 # a = q1;"
              , "q1 # b = q2;"
              , "q2 # a = q2;"
              , "q2 # b = q2"
              , "}"
              , "}"]

-- exampleDFA1 :: DFA
-- exampleDFA1 = uu ex1 ex2
--      where
--        ex1 = snd $ head $ evaluate exampleStr1
--        ex2 = snd $ head $ evaluate exampleStr2
--
-- exampleDFA2 :: DFA
-- exampleDFA2 = (^@) ex1 exampleDFA1
--     where
--       ex1 = snd $ head $ evaluate exampleStr1
