module ProjectRosalind.Buyable where
--
import Test.QuickCheck
--
--import Data.Set as S
import Data.Vector as V
--
--

-- achievement: list comprehension to compute slices of all possible substrings
lengthToSlices :: Int -> [(Int, Int)]
lengthToSlices len = Prelude.concat [ [ (l, r) | l <- [0..(len - 1)], l <= r ] | r <- [(len - 1), (len - 2)..0] ]

---- achievement: list comprehension to compute start and run of all possible substrings
--lengthToStartAndRun :: Int -> [(Int, Int)]
--lengthToStartAndRun len = concat [ [ (l, (r - l) + 1) | l <- [0..(len - 1)], l <= r ] | r <- [(len - 1), (len - 2)..0] ]



-- 1. compute all possible substrings
--   in terms of: start and run (not as we currently have it: left and right)
lengthToStartAndRun :: Int -> [(Int, Int)]
lengthToStartAndRun len = Prelude.concat [ [ (l, (r - l) + 1) | l <- [0..(len - 1)], l <= r ] | r <- [(len - 1), (len - 2)..0] ]

-- 2. convert string to vector
str = "GATTACA"
vstr = V.fromList str

-- 3. use the vector generate function: 
--   Int: subCount $ length str
--   (Int -> a) 
--     Int is index into all possible substrings list
--     a is the slice out of str vector of that particular substring
--     slice: 
slices :: String -> V.Vector (V.Vector Char)
slices str = V.generate (subCount len) f
  where 
    f :: Int -> (V.Vector Char)
    f ix = V.slice start run vstr
      where
        (start, run) = startRunList !! ix

    len :: Int    
    len = Prelude.length str
    
    startRunList :: [(Int, Int)]
    startRunList = lengthToStartAndRun len
    
    vstr :: (V.Vector Char)
    vstr = V.fromList str
    
    subCount :: Int -> Int
    subCount n = (n * (n + 1)) `div` 2



-- is this fast?
-- is it fast enough?



-- Image of what build does: 
-- 0     6
-- GATTACA    0, 6
-- GATTAC     0, 5
-- GATTA      0, 4
-- GATT       0, 3
-- GAT        0, 2
-- GA         0, 1
-- G          0, 0
--  ATTACA    1, 6
--  ATTAC     1, 5
--  ATTA      1, 4
--  ATT       1, 3
--  AT        1, 2
--  A         1, 1
--   TTACA    2, 6
--   TTAC     2, 5
--   TTA      2, 4
--   TT       2, 3
--   T        2, 2
--    TACA    3, 6
--    TAC     3, 5
--    TA      3, 4
--    T       3, 3
--     ACA    4, 6
--     AC     4, 5
--     A      4, 4
--      CA    5, 6
--      C     5, 6
--       A    6, 6
 



--subCount n = (n * (n + 1)) `div` 2
--
--x = "MSRVGKYPVEVPAGVQVSVADGFFKAKGKLGELTVPVSRHVEVKIEGSNVSVAPVGRRS"
--
--y = "GATTACA"
--y' = subCount $ Prelude.length y
--
-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_buildCount :: String -> Bool
prop_buildCount str = 
  Prelude.length (lengthToSlices $ length str) == (n * (n + 1)) `div` 2
  where n = Prelude.length str
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
---- l = 0, count down from length to l + 1
---- increment l by one, count down from length to l+1
--
---- do all our dna strings have exactly the same length?
---- if they do, we could calculate the cuts once
--
---- indeed, they are all exactly 1000 characters long: 
----[1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000]
--
---- so, if we calculate the slices once
---- and then slice dna-count times 
---- how fast are we?
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--build :: String -> Set (V.Vector Char)
--build str = create (V.fromList str) (V.fromList str) S.empty
--  where 
--    create :: (Vector Char) -> (Vector Char) -> Set (Vector Char) -> Set (Vector Char)
--    create carry block acc = 
--      -- No more carry
--      -- No more block:
--      --   1. return acc
--      if carryLen == 0 && blockLen == 0
--      then
--        acc
--      -- There is more carry
--      -- No more of this block:
--      --   1. bump one off head of carry
--      --   2. new carry value placed as new block
--      --   3. insert that carry value into acc
--      else if blockLen == 0
--      then 
--        create (V.tail carry) (V.tail carry) acc
--      -- There is more carry
--      -- There is more of this block: 
--      --   1. do not touch carry
--      --   2. bump one off tail of block
--      --   3. save block to acc
--      else
--        create carry (V.init block) (S.insert block acc)
--        
--      where 
--        carryLen :: Int
--        carryLen = V.length carry
--        
--        blockLen :: Int
--        blockLen = V.length block
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
---- Carry string 
---- Copy of carry (block)
---- Next is that without last item
---- Continue while there’s any more of copy 
---- None left? Knock first from carry, loop
--
----build :: String -> [String]
----build str = create (V.fromList str) (V.fromList str) []
----  where 
----    create :: (V.Vector Char) -> (V.Vector Char) -> [V.Vector Char] -> [String]
----    create carry block acc = 
----      -- No more carry
----      -- No more block:
----      --   1. return acc
----      if carryLen == 0 && blockLen == 0
----      then
----        fmap (V.toList) acc
----      -- There is more carry
----      -- No more of this block:
----      --   1. bump one off tail of carry
----      --   2. copy that to block
----      --   3. add nothing to block
----      else if blockLen == 0 
----      then 
----        create (V.tail carry) (V.tail carry) acc
----      -- There is more carry
----      -- There is more of this block: 
----      --   1. do not touch carry
----      --   2. bump one off tail of block
----      --   3. save block to acc
----      else
----        create carry (V.init block) (block : acc)
----        
----      where 
----        carryLen :: Int
----        carryLen = V.length carry
----
----        blockLen :: Int
----        blockLen = V.length block
--
---- Image of what build does: 
--
---- GATTACA
---- GATTAC
---- GATTA
---- GATT
---- GAT
---- GA
---- G
----  ATTACA
----  ATTAC
----  ATTA
----  ATT
----  AT
----  A
----   TTACA
----   TTAC
----   TTA
----   TT
----   T
----    TACA
----    TAC
----    TA
----    T
----     ACA
----     AC
----     A
----      CA
----      C
----       A
-- 
