module Lib
    ( someFunc
    ) where

import Representation (TonalitySystem, Music)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generate :: DrawingSettings -> TonalitySystem -> [Music a b] -> Diagram B
generate d t = vcat.map (drawStaff d t)

drawStaff :: DrawingSettings -> TonalitySystem -> Music a b-> Diagram B
drawStaff d t m = circle 1

type SizeT = Rational

data DrawingSettings = DrawingSettings { lineSpacing :: SizeT }
