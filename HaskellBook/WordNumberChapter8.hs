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

-- digits :: Int -> [Int]
-- digits n = 
--   (n `mod` 10) : final n 
--   where 
--     final x = 
--       case x `quot` 10 of
--         0 -> []
--         n -> digits n

digits'' :: Int -> [Int]
digits'' n = 
  if n == 0 
  then []
  else (n `mod` 10) : digits'' (n `quot` 10)

digits' :: Int -> [Int]
digits' n = 
  (n `mod` 10) : final n 
  where 
    final x = 
      case x `quot` 10 of
        0 -> []
        w -> digits w

digits :: Int -> [Int]
digits n = reverse $ go n 
  where 
    go n = 
      (n `mod` 10) : final n 
      where 
        final x = 
          case x `quot` 10 of
            0 -> []
            n -> digits n

wordNumber :: Int -> String
wordNumber n = undefined