{-# LANGUAGE GADTs #-}

module RexpInterp
    (evaluate, example
    ) where

import Rexp
import Parsing
import RexpParser
import Data.List
import Data.Maybe
import qualified Data.Map as M



{- FA TAG -}
interpAlphabet :: Alphabet -> String
interpAlphabet (A s) = s

interpStState :: StState -> State
interpStState (Start (St s)) = State s (Just [I])

interpFStateSet :: FStateSet -> [State]
interpFStateSet (Final fs)   = map (\(St s) -> State s (Just [F])) fs

interpStateCore :: StateCore -> State
interpStateCore (St s) = State s Nothing

interpImportantStateH :: ImportantStates -> (State, [State])
interpImportantStateH (Imp s fs) = (interpStState s, interpFStateSet fs)

{- Find right initial state given list of final states -}
findInitial :: State -> [State] -> State
findInitial q0@(State s (Just [I])) fs
          |any (\(State s1 (Just [F])) -> s == s1) fs = State s (Just [I, F])
          |otherwise = q0

makeFinals ::  State -> [State] -> [State]
makeFinals (State s (Just [I])) fs =
  map (\q0@(State s1 (Just [F])) ->
       if s1 == s then State s (Just [I, F])
         else q0) fs


interpImportantStates :: ImportantStates -> (State, [State])
interpImportantStates imps = (findInitial first second, makeFinals first second)
        where
          impsH  = interpImportantStateH imps
          first  = fst impsH
          second = snd impsH

interpAllStates :: AllStates -> [State]
interpAllStates (All _ []) = []
interpAllStates (All imps@(Imp (Start q0) (Final fs)) (x@(St s):xs)) =
  if x == q0 && x `elem` fs
    then (State s (Just [I, F])): interpAllStates (All imps xs)
    else if x == q0
      then (State s (Just [I])): interpAllStates (All imps xs)
      else if x `elem` fs
        then (State s (Just [F])): interpAllStates (All imps xs)
        else (State s Nothing): interpAllStates (All imps xs)


type StateConverter = M.Map StateCore State

-- makeStateConverter :: DeltaCore -> StateConverter
-- makeStateConverter (Dt l) = map (\Tr stc ch stc1 -> ) l

makeStateConverter :: AllStates -> M.Map StateCore State
makeStateConverter alls@(All _ l) = M.fromList (zip l (interpAllStates alls))


interpTransition :: Transition -> StateConverter -> ((State, Char), State)
interpTransition (Tr s c s1) dict = -- deal with Nothing
  ((fromJust $ M.lookup s dict, c), fromJust $ M.lookup s1 dict)

interpDeltaCore :: DeltaCore -> StateConverter -> Delta
interpDeltaCore (Dt l) dict = M.fromList (map (\t -> interpTransition t dict) l)

-- might change in future to incorporate types.
interpDFA :: FA -> (String, DFA)
interpDFA (FA (ID ty ( n)) al sts@(All imps l) d) =
  (n, D (fst $ interpImportantStates imps) (interpAllStates sts) (interpAlphabet al)
  (interpDeltaCore d (makeStateConverter sts)) (snd $ interpImportantStates imps))



makeFAList :: Program -> M.Map String DFA
makeFAList fas = M.fromList [interpDFA fas]

interpProgram :: Program -> M.Map String DFA
interpProgram = makeFAList


{-------------------------------------------------------
------ Main Language is set up -------------------------
-----------Time to try out some examples------------------
---------------------------------------------------------}

example :: String
example = unlines ["dfa fstDFA = {"
          , "alphabet = ab"
          , "states = {initial = q0; final = (q1, q3); all = (q0, q1, q2, q3)}"
          , "delta = {"
          , "q0 # a = q1;"
          , "q1 # b = q2;"
          , "q2 # a = q3;"
          , "q3 # b = q0}}"]

evaluate :: String -> [(String, DFA)]
evaluate s =  case  parseSome parseProgTag s of
  Right parsedProg -> M.toList $ interpProgram (fst parsedProg)
  Left  msg              -> error (show msg)

main :: IO ()
main = print $ show $ evaluate example
