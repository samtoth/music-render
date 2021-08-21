{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import GHC.Float
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

aCircle :: Double -> Diagram B
aCircle n = circle n  # fc blue
                    # lw veryThick

diagram :: Int -> Int -> Diagram B
diagram x y = vcat (replicate y $ hcat (map alignT (map aCircle [ 1 .. int2Double x ])))

myDi :: Diagram B
myDi = let point = circle 0.8 # fc black
        in atPoints (regPoly 6 1) (repeat point)

main :: IO ()
-- main = mainWith $ vrule 25 ||| diagram 5 5
main = mainWith myDi
