-- module drawFA where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Query
import Graphics.SVGFonts
import Data.Map as M

import RexpInterp
import Rexp



drawWithName :: String -> Double -> Diagram B
drawWithName name d = text name # fontSizeL d # fc green <> circle d # fc white

drawState :: State -> Diagram B
drawState (State name stkind) = case stkind of
  Nothing     -> drawWithName name 0.2 # named name
  Just [I]    -> let cn = drawWithName name 0.2 in
                 atop (translate (V2 (-0.315) 0.0) (rotate (-90.0 @@ deg) (triangle 0.2 # fc green))) cn # named name
  Just [F]    -> let cn = drawWithName name 0.2 in atop cn (scale 1.5 (circle 0.2)) # named name
  Just [I, F] -> let cn = drawWithName name 0.2 in
                 atop (translate (V2 (-0.415) 0.0) (rotate (-90.0 @@ deg) (triangle 0.2 # fc green))) (atop cn (scale 1.5 (circle 0.2))) # named name


-- From diagrams tutorial

text' d s = (strokeP $ textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s)
           # lw none # fc black

stateLabel = text' 2
arrowLabel txt size = text' size txt

shaft  = arc xDir (-1/6 @@ turn)
shaft' = arc xDir (-2.7/5 @@ turn)
line = trailFromOffsets [unitX]

arrowStyle1 = (with  & arrowHead  .~ spike & headLength .~ normal
                     & arrowShaft .~ shaft)

arrowStyle2 = (with  & arrowHead   .~ spike
                     & arrowShaft  .~ shaft' & arrowTail .~ lineTail
                     & tailTexture .~ solid black & lengths .~ normal)

-----------------------------------------------------------

makeArrows :: Delta -> [Diagram B]
makeArrows d = fmap (\((_, c), _) -> arrowLabel (show c) 0.5) (M.toList d)

makepointZ :: Int -> (Double, Double) -> (Double, Double)
makepointZ m s@(x, y) =  if m `mod` 2 == 0 then (x + 2*(fromIntegral m), y) else (x + 2*(fromIntegral m), y - 2)

makepointOT :: Int -> (Double, Double) -> (Double, Double)
makepointOT m s@(x, y) =  if m `mod` 2 == 1 then (x + (fromIntegral (m + 1)), y + 2) else (x + (fromIntegral m), y - 2)


points :: Int -> (Double, Double) -> [(Double, Double)]
points n s@(x, y) = case n `mod` 3 of
  0 -> [makepointZ m s |m <- [0..n]]
  1 -> [if m == 0 then s else makepointOT m s| m <- [0..n]]
  2 -> [if m == 0 then s else if m == n then (x + (fromIntegral m), y) else makepointOT m s|m <- [0..n]]

diapoints n p = fmap p2 $ points n p


makeStates :: [State] -> Delta -> Diagram B
makeStates sl delta = let ds = (fmap drawState sl) ++ makeArrows delta in position (zip (diapoints (length sl) (0.0, 0.0)) ds) -- makeArrows delta ++

drawTransition :: ((State, Char), State) -> Diagram B  -> Diagram B
drawTransition ((st1, c), st2) diag = if st1 == st2 then case st1 of
                                   State str _  ->  diag # connectPerim' arrowStyle2 str str (4/12 @@ turn) (2/12 @@ turn)
                                 else case (st1, st2) of
                                  (State str1 _, State str2 _) -> diag # connectOutside' arrowStyle1 str1 str2


drawDelta :: Delta -> Diagram B -> Diagram B
drawDelta d diag = let l = M.toList d in Prelude.foldr drawTransition diag l


drawDFA :: DFA -> Diagram B
drawDFA (D _ l _ d _) = let diag = makeStates l d in drawDelta d diag


renderDiagram :: FilePath -> (Double, Double) -> QDiagram SVG V2 Double Any -> IO ()
renderDiagram path (x, y) = renderSVG path (mkSizeSpec2D (Just x) (Just y))

main :: IO ()
main = case snd $ head $ evaluate example of
  dfa@(D fstate lstates alph delta allstat) -> renderDiagram "./test.svg" (300.0, 300.0) (drawDFA dfa)
