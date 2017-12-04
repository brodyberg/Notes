module Cipher where 

import Data.Char

brody = "brody"

--  map (\c -> chr . fromIntegral $ (ord c) + modShift) cs
--   map (\c -> chr $ (ord c) + fromIntegral(modShift)) cs
--  map mapper cs

caesar :: Integer -> [Char] -> [Char]
caesar _ [] = []
caesar shift cs = map mapper cs
  where 
    modShift = mod shift 26
    -- new ord has to be from 'a'..'z' still
    -- charMod = 
    charMod n =
      case n > (ord 'z') of 
        True -> n - (ord 'z' - ord 'a') - 1 
        False -> n
--    mapper c = chr $ (ord c) + fromIntegral(modShift)
    mapper c = chr . charMod $ (ord c) + fromIntegral(modShift)
  

-- -- from a shift, get the translated letter

-- -- take the shift and mod it
-- -- grok mod, ugh

-- mod 0 26   -- 0
-- mod 27 26  -- 1
-- mod 52 26  -- 0

-- -- char
-- -- shift
-- -- mod result
-- -- base char 'a' -> new char

-- mod26 x = mod x 26

-- base = ord 'a'
-- shift = 5
-- chr $ base + mod shift 26

-- -- how to wrap
-- -- if we have 'z' getting shifted how do we wrap that 
-- -- around?
-- -- we take whatever the result is and shrink by base?