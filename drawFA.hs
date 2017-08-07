-- module drawFA where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Query

import RexpInterp
import Rexp



drawWithName :: String -> Double -> QDiagram SVG V2 Double Any
drawWithName name d = text name # fontSizeL d # fc green <> circle d # fc white

drawState :: State -> QDiagram SVG V2 Double Any
drawState (State name stkind) = case stkind of
  Nothing     -> drawWithName name 0.2
  Just [I]    -> let cn = drawWithName name 0.2 in
                 atop (translate (V2 (-0.315) 0.0) (rotate (-90.0 @@ deg) (triangle 0.2 # fc green))) cn
  Just [F]    -> let cn = drawWithName name 0.2 in atop cn (scale 1.5 (circle 0.2))
  Just [I, F] -> let cn = drawWithName name 0.2 in
                 atop (translate (V2 (-0.415) 0.0) (rotate (-90.0 @@ deg) (triangle 0.2 # fc green))) (atop cn (scale 1.5 (circle 0.2)))


renderDiagram :: FilePath -> (Double, Double) -> QDiagram SVG V2 Double Any -> IO ()
renderDiagram path (x, y) = renderSVG path (mkSizeSpec2D (Just x) (Just y))

main :: IO ()
main = renderDiagram "./test.svg" (300.0, 300.0) (drawState (State "first" (Just [I, F])))
