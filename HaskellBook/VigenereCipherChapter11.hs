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

-- so that's a fold/map: 
-- foldr (\c (index, acc) -> )

-- map across message and do simple thing
-- 

-- map (\c -> c) message
-- "we will meet at dawn, that is the plan"

-- foldr (\c (index, acc) -> (index + 1, c : acc)) (0, "") message
-- (38,"we will meet at dawn, that is the plan")

-- foldr (\c (index, modAcc, acc) -> (index + 1, (mod index (length keyword)) : modAcc, c : acc)) (0, [], "") message

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

result = foldr rotate (keyword, 0, [], "") message

-- (index + 1, (ord $ keyword !! (mod index (length keyword))) : modAcc, c : acc)

--foldr (\c (index, modAcc, acc) -> (index + 1, (ord $ keyword !! (mod index (length keyword))) : modAcc, c : acc)) (0, [], "") message

-- map across message where we consume
-- chars in the keyword in a loop

-- preserve spaces?
-- so, do the map but with the keyword in mind
-- and skip spaces

-- concat $ take 50 $ repeat keyword 
-- makes a huge [Char] of our keyword repeated...
-- but will it only ever be 
-- (length keyword) * 50 chars long?

-- endlessZip :: [a] -> [b] -> [(a, b)]
-- endlessZip []

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