{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

--(...) :: a -> (f -> a -
module Lib
  ( initialize
  ) where

import Data.Colour (withOpacity)
import System.Exit
import System.IO

import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

--import Diagrams.Backend.Canvas.CmdLine
--import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

import Data.Char (toLower)

type State = (Int, Int)

startRadius = 3.0

padding = 0.1

-- Ring of Keys; Spiral of 19 Keys in Wedges ------------------------------
keyText :: State -> [String]
keyText (solfegeDo, _) =
  map
    (\n -> (['A' ..] !! ((4 * n - 1) `mod` 7)) : alterateKey ((n + 2) `div` 7)) $
  take 19 [solfegeDo - 8 ..]

alterateKey :: Int -> String
alterateKey n
  | n < (-1) = bb ++ alterateKey (n + 2)
  | n < 0 = replicate (-n) '♭'
  | n > 1 = ss ++ alterateKey (n - 2)
  | otherwise = replicate n '♯'
  where
    bb = "♭♭" --'𝄫'
    ss = "♯♯" -- '𝄪'

key :: String -> Diagram B
key = text -- <> square 1 # lw none

keysRing :: State -> [Diagram B]
keysRing (solfegeDo, mode) =
  rotateBy (fromIntegral (-solfegeDo) / 12) .
  zipWith rotateBy [-1 / 12,-2 / 12 ..] .
  zipWith translateY [startRadius,(startRadius + spiralDelta) ..] .
  zipWith fc cs . map key $
  keyText (solfegeDo, mode)
  where
    spiralDelta = (0.8 + padding) / 12.0
    cs = replicate 6 gray ++ replicate 7 black ++ replicate 6 gray

aKeyWedge :: Diagram B
aKeyWedge = annularWedge 5.2 2.6 xDir (1 / 12 @@ turn) # rotateBy (-1 / 24)

keyWedges :: Diagram B
keyWedges = mconcat . take 12 . iterate (rotateBy (1 / 12)) $ aKeyWedge

rotateList :: Int -> [a] -> [a]
rotateList = drop <> take

keys :: State -> Diagram B
keys (solfegeDo, mode) =
  rotateBy (8 / 12) . mconcat $ keysRing (solfegeDo, mode)

-- -------------------------------------------------------------------------
-- Ring Solfege; 7 Solfege Syllables in colored wedges (3 major, 3 minor, 1 diminished)
-- could also be prolongued with "Fi", "Di" and so on
solfegeRing :: State -> Diagram B
solfegeRing (solfegeDo, mode) =
  rotateBy ((2 - fromIntegral solfegeDo) / 12) .
  mconcat .
  zipWith fc cs .
  zipWith rotateBy [-1 / 12,-2 / 12 ..] . map (translateY 5.5 . key) $
  solfegeText
  where
    solfegeText :: [String]
    solfegeText = ["Fa", "Do", "So", "re", "la", "mi", "TI"]
    cs = rotateList (6 - ((2 * mode - 1) `mod` 7)) $ replicate 6 gray ++ [black]

modesRing :: State -> Diagram B
modesRing (solfegeDo, mode) =
  rotateBy ((2 - fromIntegral solfegeDo) / 12) .
  mconcat .
  zipWith fc cs .
  zipWith rotateBy [-1 / 12,-2 / 12 ..] .
  map ((translateY 6.4 # fontSize (local 0.6)) . key) $
  modesText
  where
    modesText :: [String]
    modesText =
      [ "Lydian"
      , "Ionian"
      , "Mixolydian"
      , "Dorian"
      , "Aeolian"
      , "Phrygian"
      , "Locrian"
      ]
    cs = rotateList (6 - ((2 * mode - 1) `mod` 7)) $ replicate 6 gray ++ [black]

aSolfegeWedge :: Diagram B
aSolfegeWedge = annularWedge 7.2 5.3 yDir (1 / 12 @@ turn) # rotateBy (-1 / 24)

solfegeWedges :: State -> Diagram B
solfegeWedges (solfegeDo, mode) =
  rotateBy ((2 - fromIntegral solfegeDo) / 12) .
  mconcat . zipWith rotateBy [-1 / 12,-2 / 12 ..] . zipWith fc solfegeColors $
  repeat aSolfegeWedge
  where
    solfegeColors = [honeydew, honeydew, honeydew, bisque, bisque, bisque, pink]

-- -------------------------------------------------------------------------
-- Ring Steps; 13 Steps in 12 Wedges; the "7" wedge is colored
stepsRing :: State -> Diagram B
stepsRing (solfegeDo, mode) =
  rotateBy ((2 - fromIntegral solfegeDo) / 12) .
  mconcat .
  zipWith rotateBy [-1 / 12,-2 / 12 ..] .
  map ((translateY 7.5 # fontSize (local 1.0)) . key) $
  stepsText
  where
    stepsText =
      zipWith
        ($)
        [ id
        , id
        , id
        , map toLower
        , map toLower
        , map toLower
        , flip (++) " dim" . map toLower
        ] .
      take 7 . map (showRoman . (+ 1) . (`mod` 7)) $
      iterate (+ 4) ((4 - mode) `mod` 7)
      where
        showRoman :: Int -> String
        showRoman x
          | x < 1 || x > 7 = error "showRoman only defined for 1..7!"
          | otherwise = ["I", "II", "III", "IV", "V", "VI", "VII"] !! (x - 1)

aStepWedge :: Diagram B
aStepWedge = annularWedge 8.5 7.3 yDir (1 / 12 @@ turn) # rotateBy (-1 / 24)

stepWedges :: State -> Diagram B
stepWedges (solfegeDo, _) =
  rotateBy ((7 - fromIntegral solfegeDo) / 12) .
  mconcat . take 7 . iterate (rotateBy (1 / 12)) $
  aStepWedge

-- -------------------------------------------------------------------------
cofs :: State -> Diagram B
cofs state =
  circle 0.1 <>
  keys state <>
  keyWedges <>
  solfegeRing state <>
  modesRing state <>
  solfegeWedges state <>
  stepsRing state <> stepWedges state <> circle 10 # lw 0 # bg white

loop :: State -> IO ()
loop (step, mode) = do
  renderSVG "test.svg" (mkWidth 800) $ cofs (step, mode)
  c <- getChar
  case c of
    '+' -> loop (step + 1, mode)
    '-' -> loop (step - 1, mode)
    'm' -> loop (step, (mode `mod` 7) + 1)
    'q' -> do
      putStrLn "Bye. CU\n"
      exitSuccess

initialize :: IO ()
initialize = do
  hSetBuffering stdin NoBuffering
  putStrLn "(+) Step up. (-) Step down. (m) Cycle mode. (q)uit"
  loop (0, 1)
-- Make an SVG-Image by "stack exec -- cofs-exe -w 600 -h 600 -o test.svg"
