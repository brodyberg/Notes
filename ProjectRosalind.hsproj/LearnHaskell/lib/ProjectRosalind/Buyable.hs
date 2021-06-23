module ProjectRosalind.Buyable where

import Test.QuickCheck
  
import Data.Vector as V


subCount n = (n * (n + 1)) `div` 2

x = "MSRVGKYPVEVPAGVQVSVADGFFKAKGKLGELTVPVSRHVEVKIEGSNVSVAPVGRRS"

y = "GATTACA"
y' = subCount $ Prelude.length y

-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_buildCount :: String -> Bool
prop_buildCount str = 
  Prelude.length (build str) == (n * (n + 1)) `div` 2
  where n = Prelude.length str

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
      --   1. return acc
      if carryLen == 0 && blockLen == 0
      then
        fmap (V.toList) acc
      -- There is more carry
      -- No more of this block:
      --   1. bump one off tail of carry
      --   2. copy that to block
      --   3. add nothing to block
      else if blockLen == 0 
      then 
        create (V.tail carry) (V.tail carry) acc
      -- There is more carry
      -- There is more of this block: 
      --   1. do not touch carry
      --   2. bump one off tail of block
      --   3. save block to acc
      else
        create carry (V.init block) (block : acc)
        
      where 
        carryLen :: Int
        carryLen = V.length carry

        blockLen :: Int
        blockLen = V.length block

-- Image of what build does: 

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
 
