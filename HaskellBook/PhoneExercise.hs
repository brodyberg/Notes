module Phone where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Char (isUpper, toLower, isLetter, toUpper)

convo :: [String]
convo = [
  "Hope is the beginning of despair",
  "Ok Marneas calm down",
  "We were betrayed at Calth",
  "True enough",
  "Only the Emperor can save us now",
  "Since when do Ultramarines say that",
  "Put on your fancy gauntlets and fight"]

type Digit = Char
type Presses = Int

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

-- 1. Create a datastructure that captures the
--    phone layout on page 457.

data Phone = Phone String [String]
  deriving (Eq, Show)

standardPhone :: Phone
standardPhone
       = Phone "123456789*0#"
               ["",     "abc", "def",
                "ghi",  "jkl", "mno",
                "pqrs", "tuv", "wxyz",
                "^",    "+ ",  ".,"]

standardCharToTaps :: Char
                   -> [(Digit, Presses)]
standardCharToTaps = charToTaps standardPhone

charToTaps :: Phone
           -> Char
           -> [(Digit, Presses)]
charToTaps (Phone keys values) c =
  if isUpper c
  then ('*', 1) : (charToTaps (Phone keys values) $ toLower c)
  else thePress
  where
    f (k, v) acc =
      if elem c (v ++ [k])
      then (k, pressCount c v): acc
      else acc
    pressCount :: Char -> String -> Presses
    pressCount k v = (fromJust $ elemIndex c (v ++ [k])) + 1
    thePress = foldr f [] $ zip keys values

-- tapsToChar :: Phone
--            -> [(Digit, Presses)]
--            -> Char
-- tapsToChar (Phone keys values) taps =
--   -- need to take into account capitalization
--   let
--     (digit, presses) = taps !! 0
--     kv = zip keys values
--     filtered = filter (\(c, str) -> c == digit) kv
--     x = head filtered
--     makeTheStr (key, values) = 
--       values ++ [key]
--   in
--     (makeTheStr x) !! (presses - 1)

tapToChar :: Phone
          -> (Digit, Presses)
          -> Char
tapToChar (Phone keys values) (digit, presses) =
  let
    kv = zip keys values
    filtered = filter (\(c, str) -> c == digit) kv
    x = head filtered
    makeTheStr (key, values) = 
      values ++ [key]
  in
    (makeTheStr x) !! (presses - 1)

tapsToString :: Phone
             -> [(Digit, Presses)]
             -> String
tapsToString phone taps = 
  foldr processor (phone, False, "") taps

processor :: (Digit, Presses)
          -> (Phone, Bool, String)
          -> (Phone, Bool, String)
processor tap (p, upCase, acc) = 
  let
    tapChar = tapToChar p tap
    capitalize = tap == ('*', 1)
    outputChar = 
      if upCase && isLetter tapChar 
      then toUpper tapChar
      else tapChar
  in 
    if capitalize
    then (p, capitalize, acc)
    else (p, capitalize, outputChar : acc)

tapsa = [('2', 1 :: Presses)]
tapsA = [('*', 1 :: Presses), ('2', 1 :: Presses)]



(Phone keys values) = standardPhone
kv = zip keys values

standardStringToTaps :: String
                     -> [(Digit, Presses)]
standardStringToTaps = stringToTaps standardPhone

stringToTaps :: Phone
             -> String
             -> [(Digit, Presses)]
stringToTaps p s = foldr (++) [] $ map (\c -> charToTaps p c) s
-- standardStringToTaps $ convo !! 1
-- [('*',1),('6',3),('5',2),('0',2),('*',1),('6',1),('2',1),('7',3),('6',2),('3',2),('2',1),('7',4),('0',2),('2',3),('2',1),('5',3),('6',1),('0',2),('3',1),('6',3),('9',1),('6',2)]

