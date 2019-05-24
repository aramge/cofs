{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ParallelListComp          #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour (withOpacity)

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
                | t <- [3, 3.07..]] -- !!! Delta should be found automaticall from fontSize/12 + padding

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
modesRing  = mconcat [ key s # translateY 6.4 # rotateBy (-n/12) # fontSize (local 0.6)
                      | s <- modesText
                      | n <- [1..]
                      ]

solfegeColors =  [honeydew, honeydew, honeydew, bisque, bisque, bisque, pink]
aSolfegeWedge :: Diagram B
aSolfegeWedge = annularWedge 7 5.1 yDir (1/12 @@ turn) # rotateBy (-1/24)
solfegeWedges :: Diagram B
solfegeWedges = mconcat [ aSolfegeWedge
                          # rotateBy (-n/12)
                          # fc c
                        | n <- [1..]
                        | c <- solfegeColors
                        ]
-- -------------------------------------------------------------------------
-- Ring Steps; 13 Steps in 12 Wedges; the "7" wedge is colored
stepsText = ["III-IV", "I", "V", "II", "VI", "III", "VII", "IV", "I", "V", "II", "VI"]
stepsRing =  mconcat [ key s # translateY 7.6 # rotateBy (-n/12)
                      | s <- stepsText
                      | n <- [1..]
                      ]
aStepWedge :: Diagram B
aStepWedge = annularWedge 8.3 7.1 yDir (1/12 @@ turn) # rotateBy (-1/24)
stepWedges :: Diagram B
stepWedges = mconcat [ aStepWedge 
                       # rotateBy (-n/12)
                     | n <- [1..12]
                     ]
-- -------------------------------------------------------------------------
cofs :: Diagram B
cofs = circle 0.1 <> keysRing <> keyWedges <> solfegeRing <> modesRing <> solfegeWedges <> stepsRing <> stepWedges
main = mainWith cofs

