module VigenereCipher where

import Data.Char (ord, chr)
import Prelude

-- 1. Pick a keyword

-- 2. Match chars of message against 
--    chars of the keyword

-- 3. Index of keyword char matched against
--    the message char is the amount by which
--    to shift the message char

data Direction = 
    Backward 
  | Forward
  deriving (Eq, Show)

encode :: String
       -> String
       -> String
encode keyword message = encodedMessage
  where 
    (_, _, _, encodedMessage) = 
      foldr rotate (Forward, keyword, 0, "") message  

decode :: String
       -> String
       -> String
decode keyword message = decodedMessage
  where 
    (_, _, _, decodedMessage) = 
      foldr rotate (Backward, keyword, 0, "") message  
     
isAlpha :: Char -> Bool
isAlpha c = 
  if ord c >= ord 'a'
  then if ord c <= ord 'z' then True else False
  else False

rotate :: Char 
  -> (Direction, String, Int, String) 
  -> (Direction, String, Int, String)
rotate c (direction, keyword, index, acc) = 
  (direction, keyword, newIndex, encodedChar : acc)
  where 
    newIndex = if isAlpha c then index + 1 else index
                 -- don't update the index if nonalpha
                 -- which means we split the keyword across
                 -- spaces and punctuation
    matchedKeyChar = keyword !! (mod index (length keyword))
    matchedKeyCharOrd = ord matchedKeyChar
    -- this # has to represent the offset of this char from 'a'
    rawShiftedOrd = fromIntegral $ mod (matchedKeyCharOrd - ord 'a') 26
    shiftedOrd Forward = rawShiftedOrd
    shiftedOrd Backward = (-1) * rawShiftedOrd
    wrapOrd n = n - (ord 'z' - ord 'a') - 1
    charMod n
      | n > ord 'z' = wrapOrd n
      | otherwise   = n
    firstCodedOrd = (ord c) + shiftedOrd(direction)
    finalCodedOrd = charMod firstCodedOrd
    encodedChar = if isAlpha c then chr finalCodedOrd else c 
      -- don't encode if nonalpha

-- amazing test: 
--encode "a" "a"
-- encoded output should be: "a"

keyword = "brodybtest"
message' = "vulka fenryka to tizca"
message = "vulkafenrykatotizca"