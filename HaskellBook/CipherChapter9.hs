module Cipher where 

import Data.Char

brody = "brody"

-- caesar (-10) brody
-- "rheto"
-- caesar 10 "rheto"
-- "brody"

caesar :: Integer -> [Char] -> [Char]
caesar _ [] = []
caesar shift cs = map mapper cs
  where 
    modShift = mod shift 26
    charMod n =
      case n > (ord 'z') of 
        True -> n - (ord 'z' - ord 'a') - 1 
        False -> n
    mapper c = chr . charMod $ (ord c) + fromIntegral(modShift)