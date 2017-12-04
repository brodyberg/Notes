module Cipher where 

import Data.Char

brody = "brody"

-- caesar (-10) brody
-- "rheto"
-- caesar 10 "rheto"
-- "brody"
-- caesar 10 $ caesar (-10) brody
-- "brody"

caesar :: Integer -> [Char] -> [Char]
caesar shift = map mapper
  where 
    modShift  = fromIntegral $ mod shift 26
    wrapOrd n = n - (ord 'z' - ord 'a') - 1
    charMod n
      | n > ord 'z' = wrapOrd n
      | otherwise   = n
    mapper c = chr . charMod $ (ord c) + modShift