module ProjectRosalind.FindingASharedMotif where
  
import Data.Vector as V
import Data.Set as S
--import Data.IntMap.Strict as M

import Data.Hashable (hash)


import ProjectRosalind.Fasta (parseFasta')
import ProjectRosalind.Fasta_Types 
import System.IO (openFile, hGetContents, IOMode(ReadMode))
import Control.Monad

import Data.Time

lengthToStartRunList :: Int -> [(Int, Int)]
lengthToStartRunList len = 
  Prelude.concat 
    [[ (l, (r - l) + 1) 
      | l <- [0..(len - 1)], l <= r ] 
      | r <- [(len - 1), (len - 2)..0] ]

-- precompute
-- do work 1 rather than N times
startRunListForLength1000 = lengthToStartRunList 1000

substringsForLength :: Int -> Int
substringsForLength n = (n * (n + 1)) `div` 2

-- precompute
-- length is identical for all fasta samples
-- do work 1 rather than N times
substringsForLength1000 = substringsForLength 1000

-- 2. convert string to vector
--str = "GATTACA"
--vstr = V.fromList str


-- 3. use the vector generate function: 
--   Int: subCount $ length str
--   (Int -> a) 
--     Int is index into all possible substrings list
--     a is the slice out of str vector of that particular substring
--     slice: 

--
--Observed: 
--  Ss count for a 1k string is ~500k
--  Intersecting two 500k sets is slow
--  Intersection of two DNA’s results in ~1% the size 
--
--Hypothesis:
--  If from after this “seed” intersection we can create very tiny sets but guarantee we get all ss of length equal or less than the longest item in the intersection we may have a path forward on improving the performance 
--
--Pseudo code: 
--
--Dna1 all ss into set
--DNA 2 all ss into set
--Intersect
--Get longest string
--Next dna: filter all ss slices to be <= longest string in intersection
--Build vector of string based on the length of this filtered slice list
--Add to set 
--Intersect
--Repeat
--  Pass along progressively filtered slice list 

dnasToCommonSubstrings :: [String] -> String -- Set String
dnasToCommonSubstrings strs = longestXYsubstring
  where
    xSet = slicesSet $ strs !! 0
    ySet = slicesSet $ strs !! 1
    xyIntersection = S.intersection xSet ySet
    -- what does findMax do really?
    longestXYsubstring = S.findMax xyIntersection
    

 
  --Dna1 all ss into set
  --DNA 2 all ss into set
  --Intersect
  --Get longest string














slicesSet :: String -> Set String
slicesSet str = vectorToSet $ generateVector len f
  where 
    vectorToSet :: Vector String -> Set String
    vectorToSet = V.foldr (\i acc -> S.insert i acc) S.empty
    
    generateVector :: Int -> (Int -> String) -> Vector String
    --                              FILTER
    generateVector len = V.generate substringsForLength1000-- (substringsForLength len)

    f :: Int -> String
    f ix = V.toList $ V.slice start run vstr
      where
                       -- USE CHAINED FILTERED
        (start, run) = startRunListForLength1000 !! ix

    len :: Int    
    len = Prelude.length str
    
--    startRunList :: [(Int, Int)]
--    startRunList = lengthToStartRunList len
--    
    vstr :: Vector Char
    vstr = V.fromList str
    
--    substringsForLength :: Int -> Int
--    substringsForLength n = (n * (n + 1)) `div` 2
--
--    lengthToStartRunList :: Int -> [(Int, Int)]
--    lengthToStartRunList len = 
--      Prelude.concat 
--        [[ (l, (r - l) + 1) 
--          | l <- [0..(len - 1)], l <= r ] 
--          | r <- [(len - 1), (len - 2)..0] ]


--slices :: String -> IntMap String
--slices str = vectorToIntMap $ generateVector len f
--  where 
--    vectorToIntMap :: (Vector String) -> IntMap String
----    vectorToIntMap = V.foldr (\i acc -> M.insert (hash i) i acc) M.empty 
--    vectorToIntMap = V.foldr insert M.empty 
--    
--    insert i acc = 
--      if M.notMember hashed acc
--      then
--        M.insert hashed i acc
--      else
--        acc
--      where 
--        hashed = hash i
--  
--    -- we could do M.fromList [(Key, a)]
--
--    generateVector :: Int -> (Int -> String) -> (Vector String)
--    generateVector len = V.generate substringsForLength1000
--
--    f :: Int -> String
--    f ix = V.toList $ V.slice start run vstr
--      where
--        (start, run) = startRunListForLength1000 !! ix
--
--    len :: Int    
--    len = Prelude.length str
--        
--    vstr :: (V.Vector Char)
--    vstr = V.fromList str


fileName = "/Users/brodyberg/Documents/GitHub/Notes/ProjectRosalind.hsproj/LearnHaskell/FindingASharedMotif/rosalind_lcsm_2.txt"

readLocalFile :: String -> IO String
readLocalFile path = do  
        handle <- openFile path ReadMode
        contents <- hGetContents handle
        return contents

filePathToFastas :: String -> IO [FastaSequence]
filePathToFastas path = do
  contents <- readLocalFile path
  return $ parseFasta' contents
  
theoreticalSubstringCount :: String -> Int 
theoreticalSubstringCount s = (n * (n + 1)) `div` 2
  where n = Prelude.length s

mainSlices :: IO ()
mainSlices = do

    putStrLn "START: Making list of all fastas" 
    now <- getZonedTime
    putStrLn $ show now
   
    fastas <- filePathToFastas fileName
    putStrLn "length: "
    putStrLn $ show $ Prelude.length fastas

    now <- getZonedTime
    putStrLn "DONE: Making list of 100 fastas" 
    putStrLn $ show now
    
--    let dnas = Prelude.take 1 $ fmap fastaSeq fastas
    let dnas = fmap fastaSeq fastas

--    putStrLn $ show $ fmap Prelude.length dnas

    now <- getZonedTime
    putStrLn "START: common substrings on 2 fastas" 
    putStrLn $ show now
      
    -- https://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program 

    let ll = dnasToCommonSubstrings dnas

    putStrLn "item: " 
    putStrLn ll


--    let maps = fmap slices dnas

--    let sizes = fmap M.size maps

--    let x = fmap S.size sets
    
--    putStrLn "sizes of each: " 
--    putStrLn $ show sizes

--    let results = Prelude.foldr S.intersection (Prelude.head sets) (Prelude.tail sets) 

--    let xcount = V.length $ vectors !! 0

--    putStrLn "count of substrings from first: " 
--    putStrLn $ show xcount
    
--    putStrLn "first substring of each: " 
  
--    let size = S.size results
    
--    putStrLn "size of results: "
--    putStrLn $ show size

    now <- getZonedTime
    putStrLn "END: common substrings on 2 fastas" 
    putStrLn $ show now

  

--    now <- getZonedTime
--    putStrLn "START: intersecting all dna" 
--    putStrLn $ show now
--
--
----    let final = Prelude.foldr S.intersection (Prelude.head sets) (Prelude.tail sets) 
--  
--    putStrLn "result: " 
----    putStrLn $ show $ Prelude.length final
--    
--    now <- getZonedTime
--    putStrLn "END: intersecting all dna" 
--    putStrLn $ show now
--
    putStrLn "Done"
