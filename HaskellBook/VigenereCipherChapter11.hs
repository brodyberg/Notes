module VigenereCipher where

import Data.Char (ord, chr)
import Prelude

-- 1. Pick a keyword

-- 2. Match chars of message against 
--    chars of the keyword

-- 3. Index of keyword char matched against
--    the message char is the amount by which
--    to shift the message char

rotate :: Char 
       -> (String, Int, [Int], [Int], [Int], String) 
       -> (String, Int, [Int], [Int], [Int], String)
rotate c (keyword, index, modAcc, firstCodedOrds, finalCodedOrds, acc) = 
  (keyword, newIndex, shiftedOrd : modAcc, firstCodedOrd : firstCodedOrds, finalCodedOrd : finalCodedOrds, encodedChar : acc)
  where 
    newIndex = index + 1
    shiftedOrd = fromIntegral $ mod (ord $ keyword !! (mod index (length keyword))) 26
    --     shiftedOrd = ord $ keyword !! (mod index (length keyword))
    wrapOrd n = n - (ord 'z' - ord 'a') - 1
    charMod n
      | n > ord 'z' = wrapOrd n
      | otherwise   = n
    firstCodedOrd = (ord c) + shiftedOrd
    finalCodedOrd = charMod firstCodedOrd        
    encodedChar = 
      if ord c <= (ord 'z') || ord c >= (ord 'a')  
      then chr finalCodedOrd -- chr . finalCodedOrd -- charMod firstCodedOrd-- $ (ord c) + shiftedOrd
      else c -- don't encode if nonalpha

keyword = "brodybtest"
message = "vulka fenryka to tizca"

(_, ix, mods, firstCodedOrds, finalCodedOrds, codeMessage) = 
  foldr rotate (keyword, 0, [], [], [], "") message

shiftsInRange :: [Int] -> Bool
shiftsInRange = all (\x -> x <= 25 && x >= 0)

rangeAlpha :: [Int] -> Bool
rangeAlpha = all (\x -> x == 32 || (x <= (ord 'z') && x >= (ord 'a')))