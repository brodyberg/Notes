module VigenereCipher where

import Data.Char (ord, chr)
-- uhhh
import Prelude
--import GHC.List

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
       -> (String, Int, [Int], [Int], [Int], String) 
       -> (String, Int, [Int], [Int], [Int], String)
rotate c (keyword, index, modAcc, firstCodedOrds, finalCodedOrds, acc) = 
  (keyword, newIndex, shiftedOrd : modAcc, firstCodedOrd : firstCodedOrds, finalCodedOrd : finalCodedOrds, encodedChar : acc)
  where 
    newIndex = index + 1
    shiftedOrd = ord $ keyword !! (mod index (length keyword))
    wrapOrd n = n - (ord 'z' - ord 'a') - 1
    charMod n
      | n > ord 'z' = wrapOrd n
      | otherwise   = n
    firstCodedOrd = (ord c) + shiftedOrd
    finalCodedOrd = charMod firstCodedOrd
    encodedChar = chr finalCodedOrd -- chr . finalCodedOrd -- charMod firstCodedOrd-- $ (ord c) + shiftedOrd

keyword = "brodybtest"
message = "we will meet at dawn, that is the plan"

(_, ix, mods, firstCodedOrds, finalCodedOrds, codeMessage) = foldr rotate (keyword, 0, [], [], [], "") message

-- this makes total sense given that the keyword is entirely 
-- in a-z
-- *VigenereCipher> any (\x -> x > (ord 'z') || x < (ord 'a')) mods
-- False

rangeOk :: [Int] -> Bool
rangeOk = all (\x -> x > (ord 'z') || x < (ord 'a'))
-- lol, 
-- rangeOk $ map ord message
-- True
-- should be false: 32 is right in the list
-- any isn't what I wanted, it was all

-- now: 
-- rangeOk $ map ord message
-- False

-- reversedMessage = revese codeMessage

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