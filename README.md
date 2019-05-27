# cofs
Circle of Fifths in Haskell

> map (\n -> ((!!) ['A'..] $ mod (4*n-1) 7, floor $ fromIntegral (n+2) / 7)) [-17..19]
