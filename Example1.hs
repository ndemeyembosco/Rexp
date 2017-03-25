module Example1 where
import Rexp
import qualified Data.Map as M


{- Example DFA's -}

example1 :: DFA
example1 = D q0 q al delta finals
   where
     q0 = State "q0" (Just [I, F])
     al = "10"

     delta = M.fromList [((q0, '0'), q0), ((q0, '1'), q1)
                      ,((q1, '0'), q2), ((q1, '1'), q0)
                      ,((q2, '0'), q1), ((q2, '1'), q2)]
     q = [q0, q1, q2]

     q1 = State "q1" Nothing
     q2 = State "q2" Nothing


     finals = [q0]

example2 :: DFA
example2 = D q0 q al delta finals
   where
     q0 = State "s0" (Just [I])

     q  = [q0, q1, q2]

     q1 = State "s1" (Just [F])
     q2 = State "s2" Nothing

     al = "ab"

     delta = M.fromList [((q0, 'a'), q1), ((q0, 'b'), q0)
                      ,((q1, 'a'), q1), ((q1, 'b'), q2)
                      , ((q2, 'a'), q2), ((q2, 'b'), q2)]

     finals = [q1]

example3 :: DFA
example3 = uu example1 dfa1

example4 :: DFA
example4 = (@@) example2 dfa2
