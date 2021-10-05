module Lib
    ( someFunc,
      generate,
      DrawingSettings,
      lineSpacing

    ) where

--import Representation (TonalitySystem, Music)
import AltRep (Music)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.Default

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generate :: DrawingSettings ->  [Music a b] -> Diagram B
generate d = vcat.map (drawStaff d)

drawStaff :: DrawingSettings ->  Music a b-> Diagram B
drawStaff d m = example $ lineSpacing d

line = strokeT . fromOffsets $ [unitX]
example s = vcat' (with & sep .~ s) (replicate 5 line)

type SizeT = Double

newtype DrawingSettings = DrawingSettings { lineSpacing :: SizeT }

instance Default DrawingSettings where
    def = DrawingSettings {lineSpacing = 0.1}
