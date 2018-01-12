module VigenereCipher where

import Data.Char (ord, chr)
import Prelude

-- 1. Pick a keyword

-- 2. Match chars of message against 
--    chars of the keyword

-- 3. Index of keyword char matched against
--    the message char is the amount by which
--    to shift the message char

data Direction = Backward | Forward

encode :: String
       -> String
       -> (Direction, String, Int, [Int], [Int], [Int], String)
encode keyword message = 
  foldr rotate (Forward, keyword, 0, [], [], [], "") message  

decode :: String
       -> String
       -> (Direction, String, Int, [Int], [Int], [Int], String)
decode keyword message = 
  foldr rotate (Backward, keyword, 0, [], [], [], "") message  
     
rotate :: Char 
       -> (Direction, String, Int, [Int], [Int], [Int], String) 
       -> (Direction, String, Int, [Int], [Int], [Int], String)
rotate c (direction, keyword, index, modAcc, firstCodedOrds, finalCodedOrds, acc) = 
  (direction, keyword, newIndex, shiftedOrd : modAcc, firstCodedOrd(direction) : firstCodedOrds, finalCodedOrd : finalCodedOrds, encodedChar : acc)
  where 
    newIndex = index + 1
    shiftedOrd = fromIntegral $ mod (ord $ keyword !! (mod index (length keyword))) 26
    wrapOrd n = n - (ord 'z' - ord 'a') - 1
    charMod n
      | n > ord 'z' = wrapOrd n
      | otherwise   = n
    firstCodedOrd Forward = (ord c) + shiftedOrd
    firstCodedOrd Backward = (ord c) - shiftedOrd
    finalCodedOrd = charMod (firstCodedOrd direction)
    encodedChar = 
      if ord c <= (ord 'z') || ord c >= (ord 'a')  
      then chr finalCodedOrd
      else c -- don't encode if nonalpha

keyword = "brodybtest"
message = "vulka fenryka to tizca"

(_, _, _, codeMods, firstCodedOrds, finalCodedOrds, codedMessage) = 
  encode keyword message

(_, _, _, decodeMods, firstDecodedOrds, finalDecodedOrds, decodedMessage) = 
  decode keyword codedMessage

shiftsInRange :: [Int] -> Bool
shiftsInRange = all (\x -> x <= 25 && x >= 0)

rangeAlpha :: [Int] -> Bool
rangeAlpha = all (\x -> x == 32 || (x <= (ord 'z') && x >= (ord 'a')))