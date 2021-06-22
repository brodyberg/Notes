module ProjectRosalind.Buyable where
  
import Data.Vector as V

--import Data.Array((!), listArray)
--import qualified Data.Array as A ((!), listArray)

--a = listArray (0,1) ["Hello", "World"]

--buyable :: Int -> Bool
--buyable n = r!n
--  where
--    r = listArray (0,n) (map f [0..n])
--    f i = i == 0 || i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)
    
subCount n = (n * (n + 1)) `div` 2

x = "MSRVGKYPVEVPAGVQVSVADGFFKAKGKLGELTVPVSRHVEVKIEGSNVSVAPVGRRS"

y = "GATTACA"
y' = subCount $ Prelude.length y

--x = fmap (y !!) [0..3]
--x


























-- Carry string 
-- Copy of carry (block)
-- Next is that without last item
-- Continue while there’s any more of copy 
-- None left? Knock first from carry, loop

build :: String -> [String]
build str = create (V.fromList str) (V.fromList str) []
  where 
    create :: (V.Vector Char) -> (V.Vector Char) -> [V.Vector Char] -> [String]
    create carry block acc = 
      -- No more carry
      -- No more block:
      --   return acc
      if carryLen == 0 && blockLen == 0
      then
        fmap (V.toList) acc
      -- There is more carry
      -- No more of this block:
      --   bump one off head of carry
      --   copy that to block
      --   add nothing to block
      else if blockLen == 0 
      then 
        create (V.tail carry) (V.tail carry) acc
      -- There is more carry
      -- There is more of this block: 
      --   Bump one off the tail of this block
      --   and save that to acc
      else
        create carry (V.init block) ((V.init block) : acc) 
        
      where 
        carryLen :: Int
        carryLen = V.length carry

        blockLen :: Int
        blockLen = V.length block
        
  


--    -- Carry string 
--    vstr :: V.Vector Char
--    vstr = V.fromList str
  
    








































maker :: String -> [String]
maker str = construct len 0 (len - 1) [str] 1 0
  where 
    len = Prelude.length str    

               --length l      r      results     ix     vlook
    construct :: Int -> Int -> Int -> [String] -> Int -> Int -> [String]
    construct len l r results ix vlook = 
      if l == (len - 1)
      then results
      else if (l == r)
        then construct len (l + 1) (len - 1) results' (ix + 1) vlook
      else
        construct len l (r - 1) results' (ix + 1) (vlook + 1)
        
      -- we need to use vlook to pull back a string to build from here
      -- we need to zero it out when we do that

      where 
        
        -- needs to knock one off the back
        results' :: [String]
        results' = (results !! (ix - 1)) : results 
        
        substring :: Int -> Int -> String -> String
        substring l r s = result

          where 
            (result, _, _, _) = Prelude.foldr slice ("", l, r, 0) s            

            slice :: Char -> (String, Int, Int, Int) -> (String, Int, Int, Int)
            slice item (subStr, l, r, ix) = 
              if (ix >= l && ix <= r) 
              then (item : subStr, l, r, ix + 1) 
              else (subStr, l, r, ix + 1)

--foldr (\item (subStr, l, r, ix) -> if (ix >= l && ix <= r) then (item : subStr, l, r, ix + 1)  else (subStr, l, r, ix + 1)) ("", 2, 4, 0) y



-- WHEN WE INCREMENT L THE COUNT OF LAST PRODUCED
-- SUBSTRINGS IS OUR LOOKBACK TO GET THE FOUNDATION 
-- STRING FOR NEXT ROUND

-- GATTACA
-- GATTAC
-- GATTA
-- GATT
-- GAT
-- GA
-- G
--  ATTACA
--  ATTAC
--  ATTA
--  ATT
--  AT
--  A
--   TTACA
--   TTAC
--   TTA
--   TT
--   T
--    TACA
--    TAC
--    TA
--    T
--     ACA
--     AC
--     A
--      CA
--      C
--       A
 


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

--allSubstrings :: String -> [String]
--allSubstrings [] = []
--allSubstrings strings = allSubstrings' strings []
--  where 
--    allSubstrings' [] acc = acc
--    allSubstrings' str acc = (substrings str) ++ (allSubstrings' (tail str) [])
--
---- do everything here: https://wiki.haskell.org/Dynamic_programming_example
--
--substrings :: String -> [String]
--substrings []   = []
--substrings item = ss "" item []
--  where 
--    ss [] [] acc = acc
--    ss [] therest acc = ss [(head therest)] (tail therest) acc
--    ss save [] acc = save : acc 
--    ss save therest acc = ss (save ++ [(head therest)]) (tail therest) (save : acc)
--
--
---- Formula from
---- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
--prop_allPossibleSubstringCount :: String -> Bool
--prop_allPossibleSubstringCount str = 
--  length (allSubstrings str) == (n * (n + 1)) `div` 2
--  where n = length str
