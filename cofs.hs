-- Common constants
innerradius = 100
linewidth = 3
padding = 5
charbox = 12
data Tonality = Dur | Moll | Diminished

-- Roman Numerals Ring

romans :: [Int]
romans = [ 4, 1, 5, 2, 6, 3, 7, 4, 2, 5, 3, 6, 4]



keys = ["Cb", "Gb", "Db", "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B", "F#", "C#", "G#", "D#", "A#", "E#"]
-- numofkey = 19
numofkeys = length keys


main = do
  -- r = innerradius
  circle innerradius
  -- key ring
  -- r = r + padding + charbox/2
  romans !! 0
  -- r = r + (padding + charbox) / 12
  
  charR = r 
