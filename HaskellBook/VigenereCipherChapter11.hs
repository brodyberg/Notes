module VigenereCipher where

import Data.Char (ord)


-- 1. Pick a keyword

-- 2. Match chars of message against 
--    chars of the keyword

-- 3. Index of keyword char matched against
--    the message char is the amount by which
--    to shift the message char

-- the other way to look at this is as the 
-- index of the current message char mod'd
-- to the keyword char, so we'd never make a
-- a zip of the two but rather use mod to 
-- index into the keyword and then caesar 
-- shift by that amount

-- rotate :: Num a 
--        => Char 
--        -> ([Char], a, [a], [Char]) 
--        -> ([Char], a, [a], [Char])
rotate :: Char 
       -> ([Char], Int, [Int], [Char]) 
       -> ([Char], Int, [Int], [Char])
rotate c (keyword, index, modAcc, acc) = 
  (keyword, newIndex, shiftedOrd : modAcc, c : acc)
  where 
    newIndex = index + 1
    shiftedOrd = ord $ keyword !! (mod index (length keyword))

keyword = "brodybtest"
message = "we will meet at dawn, that is the plan"

(_, _, _, codeMessage) = foldr rotate (keyword, 0, [], "") message

-- vigenere :: [Char] -> [Char] -> [Char]
-- vigenere keyword = map mapper
--   where 
--     modShift  = fromIntegral $ mod shift 26
--     wrapOrd n = n - (ord 'z' - ord 'a') - 1
--     charMod n
--       | n > ord 'z' = wrapOrd n
--       | otherwise   = n
--     mapper ' ' = ' ' -- preserve spaces
--     mapper c = chr . charMod $ (ord c) + modShift