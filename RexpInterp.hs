{-# LANGUAGE GADTs #-}

module RexpInterp
    (
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
-- interpDFA :: FA -> (String, DFA)
-- interpDFA (FA (ID ty (Nm n)) al sts@(All imps l) d) =
--   (n, D (fst $ interpImportantStates imps) (interpAllStates sts) (interpAlphabet al)
--   (interpDeltaCore d M.empty) (snd $ interpImportantStates imps))

interpDFA :: FA -> (String, DFA)
interpDFA (FA (ID ty (Nm n)) al sts@(All imps l) d) =
  (n, D (fst $ interpImportantStates imps) (interpAllStates sts) (interpAlphabet al)
  (interpDeltaCore d (makeStateConverter sts)) (snd $ interpImportantStates imps))


type Env = M.Map String DFA
{- Statements Tag -}
interpUniOp :: UniOp -> Either DFA NFA -> Either DFA NFA
interpUniOp Neg  (Left  d) = Left $ compDFA d
--interpUniOp Conv (Right n) = undefined

{-! As of now, only DFA's work. In the future, maybe add Either DFA NFA as well-}

interpBinOp :: BinOp -> DFA -> DFA -> DFA
interpBinOp Plus d1 d2  = d1 `uu` d2
interpBinOp Times d1 d2 = (@@) d1 d2


makeEnv :: [(String, DFA)] -> M.Map String DFA
makeEnv  l = M.fromList l

interpRexpr :: RexpExpr -> Env -> DFA
interpRexpr (Autom (Nm n))  e   = fromJust $ M.lookup n e
interpRexpr (Var s)         e   = fromJust (M.lookup s e)
interpRexpr (Uni Neg r1)    e   = compDFA $ interpRexpr r1 e
interpRExpr (Bin op r1 r2)  e   = interpBinOp op (interpRexpr r1 e) (interpRexpr r2 e)



--
interpStmt :: Stmt -> M.Map String DFA -> (String, Maybe DFA)
interpStmt (Decl _ s)        e = (s, Nothing)     -- ignore types for now
interpStmt (Assign _ s r)    e = (s, Just $ interpRexpr r e)


interpStatements :: Statements -> M.Map String DFA -> [(String, Maybe DFA)]
interpStatements (Stmts l) e = map (\s -> interpStmt s e) l


makeFAList :: Program -> M.Map String DFA
makeFAList (P fas) = M.fromList (map interpDFA fas)

interpProgram :: Program -> M.Map String DFA
interpProgram prog@(P fas) = makeFAList prog


{-------------------------------------------------------
------ Main Language is set up -------------------------
-----------Time to try out some examples------------------
---------------------------------------------------------}

example1 :: String
example1 = unlines [ "<rexp>"
              , "<FA type=DFA name=multOf3>"
              , "<alphabet>10</alphabet>"
              , "<states starting=S0 final=S0>"
              , "{S0, S1, S2}"
              , "</states>"
              , "<delta>"
              , "<transition>from S0 with 0 to S0</transition>;"
              , "<transition>from S0 with 1 to S1</transition>;"
              , "<transition>from S1 with 0 to S2</transition>;"
              , "<transition>from S1 with 1 to S0</transition>;"
              , "<transition>from S2 with 0 to S1</transition>;"
              , "<transition>from S2 with 1 to S2</transition>"
              , "</delta>"
              , "</FA>,"
              , "<FA type=DFA name=endsWith1>"
              , "<alphabet>10</alphabet>"
              , "<states starting=Q0 final=Q1, Q3>"
              , "{Q0, Q1, Q2, Q3}"
              , "</states>"
              , "<delta>"
              , "<transition>from Q0 with 0 to Q0</transition>;"
              , "<transition>from Q0 with 1 to Q1</transition>;"
              , "<transition>from Q1 with 0 to Q2</transition>;"
              , "<transition>from Q1 with 1 to Q0</transition>;"
              , "<transition>from Q2 with 0 to Q3</transition>;"
              , "<transition>from Q2 with 1 to Q0</transition>;"
              , "<transition>from Q3 with 0 to Q0</transition>;"
              , "<transition>from Q3 with 1 to Q0</transition>"
              , "</delta>"
              , "</FA>"
              -- , "<statements>"
              -- , "DFA first   = multOf3 + endsWith1;"
              -- , "DFA second  = multOf3 * endsWith1;"
              -- , "DFA third   = !second"
              -- , "</statements>"
              , "</rexp>"]

evaluate :: String -> [(String, DFA)]
evaluate s =  case  parseSome parseProgTag s of
  Right parsedProg -> M.toList $ interpProgram (fst parsedProg)
  _                -> error "Syntax Error"
