module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> Maybe String
digitToWord n = 
  if n < 0 || n > 9 
  then Nothing
  else Just (translate n)
  where 
    translate :: Int -> String
    translate n = 
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

digitsG :: Int -> [Int]
digitsG n = reverse $ go n
  where 
    go 0 = []
    go n = (n `mod` 10) : digitsG (n `quot` 10)

digitsF :: Int -> [Int]
digitsF 0 = []
digitsF n = (n `mod` 10) : digitsF (n `quot` 10)
      
digits'' :: Int -> [Int]
digits'' n = 
  if n == 0 
  then []
  else (n `mod` 10) : digits'' (n `quot` 10)

digits_ :: Int -> [Int]
digits_ n = 
  (n `mod` 10) : final n 
  where 
    final x = 
      case x `quot` 10 of
        0 -> []
        w -> digits_ w

wordNumber :: Int -> String
wordNumber n = undefined