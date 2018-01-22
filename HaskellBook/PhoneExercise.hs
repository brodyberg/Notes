module Phone where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Char (isUpper, toLower)

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


tapsToChar :: Phone
           -> [(Digit, Presses)]
--           -> [(Digit, String)]
           -> Char
tapsToChar (Phone keys values) taps =
  let
    (digit, presses) = taps !! 0
    kv = zip keys values
    filtered = filter (\(c, str) -> c == digit) kv
    -- (key, values) = head filtered
    x = head filtered
    makeTheStr (key, values) = 
      values ++ [key]

    -- values = "abc"
    -- key = '2'
--    theStr = values ++ [key]
  in
    (makeTheStr x) !! (presses - 1)
    --theStr !! (mod presses (length theStr)) -- mod by length
    --'d'
    --filtered

tapsa = [('2', 1)]
tapsA = [('*', 1), ('2', 1)]


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

