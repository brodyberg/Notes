module WordNumber where

import Data.List (intersperse)

carefulDigitToWord :: Int -> Maybe String
carefulDigitToWord n = 
  if n < 0 || n > 9 
  then Nothing
  else Just (digitToWord n)

-- negatives?
-- anything greater than 9?
digitToWord :: Int -> String
digitToWord n = 
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined