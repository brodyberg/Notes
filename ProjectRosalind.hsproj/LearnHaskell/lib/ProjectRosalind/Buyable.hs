module ProjectRosalind.Buyable where
  
import Data.Array((!), listArray)
--import qualified Data.Array as A ((!), listArray)

a = listArray (0,1) ["Hello", "World"]

buyable :: Int -> Bool
buyable n = r!n
  where
    r = listArray (0,n) (map f [0..n])
    f i = i == 0 || i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)
    
subCount n = (n * (n + 1)) `div` 2

x = "MSRVGKYPVEVPAGVQVSVADGFFKAKGKLGELTVPVSRHVEVKIEGSNVSVAPVGRRS"

y = "GATTACA"
y' = subCount $ length y

maker :: String -> [String]
maker str = construct len 0 (len - 1) [str] 1
  where 
    len = length str    

               --length l      r      results     ix
    construct :: Int -> Int -> Int -> [String] -> Int -> [String]
    construct len l r results ix = 
      if l == (len - 1)
      then results
      else if (l == r)
        then construct len (l + 1) (len - 1) results' (ix + 1)
      else
        construct len l (r + 1) results' (ix + 1)
        
      where 
        results' :: [String]
        results' = (results !! (ix - 1)) : results 















-- f[0] = 0
-- f[1] = 1
-- for (i = 2; i <= n; i++) {
--   f[i] = f[i-1] + f[i - 2];
-- }

-- Start left at 0
-- Right at length - 1
--
-- Count down right pointer
-- When right pointer “touches” left, increment left by one, reset right to length -1
-- Nested comprehensions?


--ss :: String -> [String] 
---- GHC.Arr.Array Int String -- [String] -- Array Int String
--ss string = lst
--  where
--    l = (n * (n + 1)) `div` 2
--    n = length string
--    lst = listArray (0,l) (map f [0..l])
--
--    f :: Int -> String
--    f 0 = show $ head string
--    f i = ""

allSubstrings :: String -> [String]
allSubstrings [] = []
allSubstrings strings = allSubstrings' strings []
  where 
    allSubstrings' [] acc = acc
    allSubstrings' str acc = (substrings str) ++ (allSubstrings' (tail str) [])

-- do everything here: https://wiki.haskell.org/Dynamic_programming_example

substrings :: String -> [String]
substrings []   = []
substrings item = ss "" item []
  where 
    ss [] [] acc = acc
    ss [] therest acc = ss [(head therest)] (tail therest) acc
    ss save [] acc = save : acc 
    ss save therest acc = ss (save ++ [(head therest)]) (tail therest) (save : acc)


-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_allPossibleSubstringCount :: String -> Bool
prop_allPossibleSubstringCount str = 
  length (allSubstrings str) == (n * (n + 1)) `div` 2
  where n = length str