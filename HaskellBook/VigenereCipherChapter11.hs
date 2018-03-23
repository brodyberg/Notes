module VigenereCipher (encode, decode) where

import Data.Char (ord, chr)
import Prelude

-- 1. Pick a keyword

-- 2. Match chars of message against 
--    chars of the keyword

-- 3. For the character in the keyword lined 
--    up against the message character, how far 
--    is that character from the beginning of the
--    alphabet? The number is the amount by which
--    you shift the message character. 
--    ex. a is a shift of 0
--    ex. z is a shift of 25

data Direction = 
    Backward 
  | Forward
  deriving (Eq, Show)

-- All encode and decode do is abstract out the user 
-- needing to use Direction and throwing away the 
-- intermediate results of the fold. Is there a better
-- way to do this without all this duplicated code?
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
     
-- broke this out because there was a tension caused because
-- I initially put all the functions within rotate, which is ok
-- but I found that it made exposing their results, effectively
-- intermediate results for the overall function, hard to do
-- The good side of that was that it caused me to produce
-- several validation functions you can see in the blame and
-- to use hoogle for the first time where I searched for 
-- [a] -> [b] -> [c] -> [(a, b, c)] to discover zip3 and *then*
-- realize there is no zip4 but implementation should be a snap. 
isAlpha :: Char -> Bool
isAlpha c = 
  if ord c >= ord 'a'
  then if ord c <= ord 'z' then True else False
  else False

-- rotate has to know the direction, and takes the entire 
-- keyword every time it is invoked. Is there a way to 
-- not have all this work within the function?
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
    wrapOrd' n = n + (ord 'z' - ord 'a') + 1
    charMod n
      | n > ord 'z' = wrapOrd n
      | n < ord 'a' = wrapOrd' n 
      | otherwise   = n
    firstCodedOrd = (ord c) + shiftedOrd(direction)
    finalCodedOrd = charMod firstCodedOrd
    encodedChar = if isAlpha c then chr finalCodedOrd else c 
      -- don't encode if nonalpha

-- amazing test: 
-- encode "a" "a"
-- "a"
-- what's amazing about this test is that it means that 
-- a caesar cipher is a monoid: 
-- items: Char
-- operator: shift
-- zero: 'a' in this case and 0 in the caesar cipher

-- more tests: 
-- encode "b" "a"
-- "b"

keyword = "brodybtest"
message' = "vulka fenryka to tizca" -- had a hard time initially
  -- figuring out what to do with spaces, punctuation and generally
  -- non-alpha characters
message = "vulkafenrykatotizca"