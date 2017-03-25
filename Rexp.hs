{-# LANGUAGE GADTs #-}

{-------------------------------------------------------------------
- A shallowly embedded dsl for computing with regular languages -
--------------------------------------------------------------------}

module Rexp (runString, DeltaN, Delta, Final(..)
   , DFA(..), NFA(..), compDFA, uu, (@@), dfa1, dfa2, runStringN, State(..)
   ) where

import qualified Data.Map as M
import Data.List
import Data.Maybe

{- Final is the data type to specify whether a given state is a final state or an
initial state, so really, it should be called initial or final but this is much
shorter -}

data Final where
  I :: Final
  F :: Final
  deriving (Show, Eq, Ord)

{- A state is just a string, except it might be an inital state,
a final state or both -}

data State where
  State :: String -> Maybe [Final] -> State
  deriving (Show, Eq, Ord)

{- The delta is a big list of things of the form
 (state, char) -> state -}

type Delta = M.Map (State, Char) State

{- By its formal definition is a five tuple
(initial state, set of states, alphabet, delta function, set of final state) -}

data DFA where
  D :: State -> [State] -> [Char] -> Delta -> [State] -> DFA
  deriving Show

{- A dfa accepts a string, if after running the string through itself, and
there are no characters in the string, the dfa is in a final state. -}

acceptStr :: String -> State -> Delta -> Maybe Bool
acceptStr [] x _ = case x of
              State _ (Just [F])  -> Just True
              State _ (Just [I, F])  -> Just True
              _ -> Just False
acceptStr (x:xs) q0 d = case M.lookup (q0, x) d of
  Just s ->  acceptStr xs s d
  Nothing -> Nothing

runString :: String -> DFA -> Bool
runString s (D q _ _ d _)  = case acceptStr s q d of
  Just True -> True
  Just False -> False
  _          -> error "something went terribly wrong!!"

{- An example of a delta to illustrate how they work
and provide insight about how one might go about
transforming it. -}

exampleDelta :: Delta
exampleDelta = M.fromList [((State "q0" (Just [I, F]), '1'), State "q1" Nothing)
                , ((State "q0" (Just [I, F]), '0'), State "q0" (Just [I, F]))
                , ((State "q1" Nothing, '1'), State "q0" (Just [I, F]))
                , ((State "q1" Nothing, '0'), State "q2" Nothing)
                , ((State "q2" Nothing, '1'), State "q2" Nothing)
                , ((State "q2" Nothing, '0'), State "q1" Nothing)]

{--------------------------------------------------------------
----------------NFAs-------------------------------------------
--------------------------------------------------------------}

{- Differently from a DFA, an nfa can go to more than
one state, or no state at all upon reading a single character,
it can also go to a state without reading a character(e-transition) but
as of now, this is not supported. -}

type DeltaN = M.Map (State, Char) (Maybe [State])

data NFA where
  N :: State -> [State] -> String -> DeltaN -> [State] -> NFA
  deriving Show


{- An NFA accepts a string if, after running the string through itself,
one of the states it might end up in, is a final state.-}

acceptStrN :: String -> [State] -> DeltaN -> Maybe Bool
acceptStrN [] x _ = case any (\s -> case s of
         State _ (Just [I, F]) -> True
         State _ (Just [F])    -> True
         _                     -> False) x of
           True -> Just True
           False -> Just False
acceptStrN (x:xs) qs d = Just ((Just True) `elem`
       (map (\q -> case M.lookup (q, x) d of
              Just (Just s) -> acceptStrN xs s d
              Just Nothing -> Nothing
              Nothing -> Nothing) qs))

runStringN :: String -> NFA -> Bool
runStringN s (N q _ _ d _)  = case acceptStrN s [q] d of
    Just True -> True
    Just False -> False
    _          -> error "something went terribly wrong!!"

{- Again, here is an example of an NFA for the purpose of
illustrating how they work. -}

exampleDeltaN :: DeltaN
exampleDeltaN = M.fromList [
   ((State "s0" (Just [I, F]), 'a'), Just [State "s1" (Just [F]), State "s2" Nothing])
   , ((State "s0" (Just [I, F]), 'b'), Nothing)
   , ((State "s1" (Just [F]), 'a'), Just [State "s1" (Just [F])])
   , ((State "s1" (Just [F]), 'b'), Nothing)
   , ((State "s2" Nothing, 'a'), Nothing)
   , ((State "s2" Nothing, 'b'), Just [State "s3" (Just [F])])
   , ((State "s3" (Just [F]), 'a'), Just [State "s2" Nothing])
   , ((State "s3" (Just [F]), 'b'), Nothing)
   ]

{-------------------------------------------------
---------- Operations on regular languages -------
--------------------------------------------------}

{- As of now, the only operations that are supported
are the complement of a dfa, the union of 2 dfas and the
intersection of 2 dfas. In the future, converting an nfa to
a dfa will be added, and so we will have a way to concatenate
2 dfa's by using an e-transition nfa and then converting this
to a dfa. Once we have concatenation, all the basic operations
will be done. We can then use Kleene's algorithm to turn
this automata into regular expressions, or vice-versa. -}

-- 1. Complement
{- Take all Final states and make them
Non-final and all non-final final -}

compState :: State -> State
compState (State s (Just [I, F])) = State s (Just [I])
compState (State s (Just [F]))    = State s Nothing
compState (State s (Just [I]))    = State s (Just [I, F])
compState (State s _)             = State s (Just [F])

compDelta :: Delta -> Delta
compDelta d = M.map (\a -> compState a) (M.mapKeys (\(s, c) -> (compState s, c)) d)

newF :: [State] -> [State] -> Maybe [State]
newF [] []     = Just []
newF [] f      = Nothing
newF (q:qs) [] = Nothing
newF qs fs     = Just (filter (`notElem` fs) qs)

{- Taking the complement of a dfa is just a matter of
modifying the transition function accordingly and extracting the
set of new states from it as well as the set of final states
and the initial State -}

compDFA :: DFA -> DFA
compDFA (D q qs al d fs) = D (fromJust (initialState (extractStates (compDelta d))))
                             (extractStates (compDelta d)) al (compDelta d) (fromJust (newF qs fs))


-- 3. Union

{- Make a new map m.map ([state], char) state that
is the result of tracking where the two dfa's are after reading
each character from the string, and then turn this
into a dfa by applying funtion that changes [state] -> state. Turn states
that in the resulting automaton contain at least one accepting states
into accepting(final) states -}

type TrackMap = M.Map ([State], Char) [State]

andhelper' :: String -> (Delta, State) -> (Delta, State) -> TrackMap -> TrackMap
andhelper' [] _ _ t = t
andhelper' (x:xs) (d1, s1) (d2, s2) t = M.union t (M.fromList
                                           ((([s1, s2], x), [fromJust (M.lookup (s1, x) d1)
                                               , fromJust (M.lookup (s2, x) d2)]) :
                                                   M.toList (andhelper' xs (d1, s1) (d2, s2) t))) -- [(state, state, char)]


{- OMG if this wasn't tricky to think!! -}
andhelper :: Int -> String -> (Delta, State) -> (Delta, State) -> TrackMap -> TrackMap
andhelper 0 s d1 d2 t = andhelper' s d1 d2 t
andhelper n (x:xs) (d1, s1) (d2, s2) t = M.union t ( M.fromList(concatMap
                                           (\(k, [s3, s4]) ->
                                              (k, [s3, s4]) : M.toList (andhelper (n-1) (x:xs) (d1, s3) (d2, s4) t))
                                                 (M.toList (andhelper' (x:xs) (d1, s1) (d2, s2) t))))



makeStateUnion :: [State] -> State
makeStateUnion l =  let list = (map (\(State s m) -> m) l ) in if elem (Just [I, F]) list then
                                State (show l) (Just [I, F]) else
                                  if elem (Just [F]) list then
                                      State (show l) (Just [F]) else
                                        if  elem (Just [I]) list then
                                          State (show l) (Just [I]) else
                                            State (show l) Nothing


makeDelta :: TrackMap -> Delta
makeDelta t = M.mapKeys (\(l, c) -> (makeStateUnion l, c)) (M.map (\l -> makeStateUnion l) t)

extractStates :: Delta -> [State]
extractStates d = map (\((s, _), s1) -> s) (M.toList d)

finals :: [State] -> [State]
finals  = filter (\(State s m) -> m == Just [I, F] || m == Just [F])

initialState :: [State] -> Maybe State
initialState [] = Nothing
initialState (x:xs) = case x of
  (State _ (Just [I])) -> Just x
  (State _ (Just [I, F])) -> Just x
  (State _ _)            -> initialState xs

uu :: DFA -> DFA -> DFA
uu (D q qs al d fs) (D q1 qs1 al1 d1 fs1) =
  let delta = (makeDelta (andhelper (length qs + length qs1) al (d, q) (d1, q1) M.empty)) in
                D (fromJust (initialState (extractStates delta))) (extractStates delta)
                al delta (finals (extractStates delta))


-- 2. Intersection

{- Same as Union, except for accepting, just
check if all of the states are accepting. -}

makeDeltaIntersect :: TrackMap -> Delta
makeDeltaIntersect t = M.mapKeys (\(l, c) ->
                            (makeStateIntersect l, c))
                               (M.map (\l -> makeStateIntersect l) t)

(@@) :: DFA -> DFA -> DFA
(@@) (D q qs al d fs) (D q1 qs1 al1 d1 fs1) =
  let delta = (makeDeltaIntersect (andhelper (length qs + length qs1) al (d, q) (d1, q1) M.empty)) in
                D (fromJust (initialState (extractStates delta))) (extractStates delta)
                al delta (finals (extractStates delta))


getM :: [State] -> [Maybe [Final]]
getM = map (\(State s m) -> m)

makeStateIntersect :: [State] -> State
makeStateIntersect l = let list = (map (\(State s m) -> case m of
                                  Just [I, F] -> True
                                  Just [F]    -> True
                                  _           -> False) l ) in case (and list,foldr (\a b -> b && (a == Just [I, F])) True (getM l) , foldr (\a b -> b && (a == (Just [I, F]) || a == (Just [I]))) True (getM l)) of
                                    (True, True, False) -> State (show l) (Just [I, F])
                                    (True, False, False) -> State (show l) (Just [F])
                                    (False, False, True) -> State (show l) (Just [I])
                                    _                    -> State (show l) Nothing



{- ------------------------------------------     ----------------------------
------------------------------------Test DFA s---------------------------
--------------------------------------------------------------------------}

q01 :: State
q01 = State "q0" (Just [I])

q1 :: [State]
q1 = [q01, State "q1" (Just [F]), State "q2" Nothing]

al1 :: String
al1 = "10"

f1 :: [State]
f1 = [State "q1" (Just [F])]

delta1 :: Delta
delta1 = M.fromList [
        ((State "q0" (Just [I]), '0'), State "q1" (Just [F]))
      , ((State "q0" (Just [I]), '1'), State "q2" Nothing)
      , ((State "q1" (Just [F]), '0'), State "q1" (Just [F]))
      , ((State "q1" (Just [F]), '1'), State "q1" (Just [F]))
      , ((State "q2" Nothing, '0'), State "q2" Nothing)
      , ((State "q2" Nothing, '1'), State "q2" Nothing)
  ]

dfa1 :: DFA
dfa1 = D q01 q1 al1 delta1 f1

-----------------------------------------------------------------------------
r01 :: State
r01 = State "r0" (Just [I])

q2 :: [State]
q2 = [r01, State "r1" (Just [F])]

f2 :: [State]
f2 = [State "r1" (Just [F])]

delta2 :: Delta
delta2 = M.fromList [
   ((r01, '0'), State "r1" (Just [F]))
  , ((r01, '1'), State "r0" (Just [I]))
  , ((State "r1" (Just [F]), '0'), State "r1" (Just [F]))
  , ((State "r1" (Just [F]), '1'), r01)
 ]

dfa2 :: DFA
dfa2 = D r01 q2 al1 delta2 f2
---------------------------------------------------------------------------------

{------Testing----------}

-- 1. Complement
compdfa1 :: DFA
compdfa1 = compDFA dfa1

compdfa2 :: DFA
compdfa2 = compDFA dfa2

-- 2. Union
unionDfa1 :: DFA
unionDfa1 = uu dfa1 dfa2

unionDfa2 :: DFA
unionDfa2 = uu compdfa1 compdfa2

-- 3. Intersection
intersectDfa1 :: DFA
intersectDfa1 = (@@) dfa1 dfa2

intersectDfa2 :: DFA
intersectDfa2 = (@@) compdfa1 compdfa2

intersectDfa3 :: DFA
intersectDfa3 = (@@) unionDfa1 unionDfa2
