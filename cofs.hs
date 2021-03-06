{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour (withOpacity)

startRadius = 3.0
padding = 0.1

-- Ring of Keys; Spiral of 19 Keys in Wedges ------------------------------
keyText = map (\n ->
     [['A'..] !! ((4*n-1) `mod` 7)] ++
     (alterateKey $ floor $ fromIntegral (n+2) / 7)
              ) [-8..10]
-- Old
-- keyText = [
--   -- "...", "B𝄫", "F♭",
--   "C♭", "G♭", "D♭", "A♭", "E♭", "B♭",
--   "F",  "C",  "G",  "D",  "A",  "E",  "B",
--   "F♯", "C♯", "G♯", "D♯", "A♯", "E♯"
--   -- ,"B𝄪", "..."
--   ]
alterateKey :: Int -> String
alterateKey n
  | n < (-1)  = '𝄫' : alterateKey (n+2)
  | n < 0     = take (-n) $ repeat '♭'
  | n > 1     = '𝄪' : alterateKey (n-2)
  | otherwise = take n $ repeat '♯'
key :: String -> Diagram B
key s = text s <> square 1 # lw none
keysRing :: Diagram B
spiralDelta = (0.8 + padding) / 12.0
keysRing = mconcat
  . zipWith rotateBy [-1/12, -2/12..]
  . zipWith translateY [startRadius, (startRadius+spiralDelta)..]
  $ map key keyText
aKeyWedge :: Diagram B
aKeyWedge = annularWedge 5 2.6 xDir (1/12 @@ turn) # rotateBy (-1/24)
keyWedges :: Diagram B
keyWedges = mconcat . take 12 . iterate (rotateBy (1/12)) $ aKeyWedge
-- -------------------------------------------------------------------------
-- Ring Solfege; 7 Solfege Syllables in colored wedges (3 major, 3 minor, 1 diminished)
solfegeText = ["Fa", "Do", "So", "re", "la", "mi", "TI"]
              -- could also be prolongued with "Fi", "Di" and so on
solfegeRing = mconcat
  . zipWith rotateBy [-1/12, -2/12..]
  . map (translateY 5.5)
  $ map key solfegeText
modesText = ["Lydian", "Ionian", "Mixolydian", "Dorian", "Aeolian", "Phrygian", "Locrian"]
modesRing = mconcat
  . zipWith rotateBy [-1/12, -2/12..]
  . map (translateY 6.4 # fontSize (local 0.6))
  $ map key modesText
solfegeColors =  [honeydew, honeydew, honeydew, bisque, bisque, bisque, pink]
aSolfegeWedge :: Diagram B
aSolfegeWedge = annularWedge 7 5.1 yDir (1/12 @@ turn) # rotateBy (-1/24)
solfegeWedges :: Diagram B
solfegeWedges = mconcat
  . zipWith rotateBy [-1/12, -2/12..]
  . zipWith fc solfegeColors
  $ repeat aSolfegeWedge
-- -------------------------------------------------------------------------
-- Ring Steps; 13 Steps in 12 Wedges; the "7" wedge is colored
stepsText = ["III-IV", "I", "V", "II", "VI", "III", "VII", "IV", "I", "V", "II", "VI"]
stepsRing = mconcat
  . zipWith rotateBy [-1/12, -2/12..]
  . map (translateY 7.6 # fontSize (local 1.0))
  $ map key stepsText
aStepWedge :: Diagram B
aStepWedge = annularWedge 8.3 7.1 yDir (1/12 @@ turn) # rotateBy (-1/24)
stepWedges :: Diagram B
stepWedges = mconcat . take 12 . iterate (rotateBy (1/12)) $ aStepWedge
-- -------------------------------------------------------------------------
cofs :: Diagram B
cofs = circle 0.1 <> keysRing <> keyWedges <> solfegeRing <> modesRing <> solfegeWedges <> stepsRing <> stepWedges <> circle 10 # lw 0
main = mainWith cofs

