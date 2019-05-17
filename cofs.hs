{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ParallelListComp          #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour (withOpacity)
-- import Data.Colour.Palette.BrewerSet
-- import Diagrams.

-- Common constants
innerradius = 100
linewidth = 3
padding = 2
charbox = 10

-- Ring of Keys; Spiral of 19 Keys in Wedges ------------------------------
keyText = [       "Cb", "Gb", "Db", "Ab", "Eb", "Bb",
            "F",  "C",  "G",  "D",  "A",  "E",  "B",
            "F#", "C#", "G#", "D#", "A#", "E#"]
key :: String -> Diagram B
key s = text s <> square 1 # lw none

keysRing :: Diagram B
keysRing = mconcat [ key s # translateY t # rotateBy (-n/12)
                | s <- keyText
                | n <- [1..]
                | t <- [3, 3.07..]]

aKeyWedge :: Diagram B
aKeyWedge = annularWedge 5 2.6 xDir (1/12 @@ turn) # rotateBy (-1/24)
keyWedges :: Diagram B
keyWedges = mconcat . take 12 . iterate (rotateBy (1/12)) $ aKeyWedge
-- -------------------------------------------------------------------------

-- Ring Solfege; 7 Solfege Syllables in colored wedges (3 major, 3 minor, 1 diminished)
solfegeText = ["Fa", "Do", "So", "re", "la", "mi", "TI"]
              -- could also be prolongued with "Fi", "Di" and so on


solfegeRing = mconcat [ key s # translateY 5.5 # rotateBy (-n/12)
                      | s <- solfegeText
                      | n <- [1..]
                      ]
modesText = ["Lydian", "Ionian", "Mixolydian", "Dorian", "Aeolian", "Phrygian", "Locrian"]
modesRing  = mconcat [ key s # translateY 6.4 # rotateBy (-n/12)
                      | s <- modesText
                      | n <- [1..]
                      ]

solfegeColors =  [green, green, green, blue, blue, blue, magenta]

aSolfegeWedge :: Diagram B
aSolfegeWedge = annularWedge 7 5.1 yDir (1/12 @@ turn) # rotateBy (-1/24)
solfegeWedges :: Diagram B
-- solfegeWedges = mconcat . take 7 . iterate (rotateBy (1/12)) $ aSolfegeWedge
solfegeWedges = mconcat [ aSolfegeWedge # rotateBy (-n/12) # fc c
                        | n <- [1..]
                        | c <- solfegeColors
                        ]
                
  
-- Ring Steps; 13 Steps in 12 Wedges; the "7" wedge is colored
stepsText = ["III-IV", "I", "V", "II", "VI", "III", "VII", "IV", "I", "V", "II", "VI"]


cofs :: Diagram B
cofs = circle 0.1 <> keysRing <> keyWedges <> solfegeRing <> modesRing <> solfegeWedges
main = mainWith cofs

