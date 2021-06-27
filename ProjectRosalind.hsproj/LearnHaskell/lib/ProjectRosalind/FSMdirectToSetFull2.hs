module ProjectRosalind.FSMdirectToSetFull2 where
  
import Data.Vector as V
import Data.Set as S
import Data.List as L

import Data.Hashable (hash)

import ProjectRosalind.Fasta_Types 
import ProjectRosalind.FileFasta (filePathToFastas)

import Data.Time

import Data.List (maximumBy)
import Data.Function (on)

lengthToStartRunList :: Int -> [(Int, Int)]
lengthToStartRunList len = 
  Prelude.concat 
    [[ (l, (r - l) + 1) 
      | l <- [0..(len - 1)], l <= r ] 
      | r <- [(len - 1), (len - 2)..0] ]

startRunListForLength1000 = lengthToStartRunList 1000

substringsForLength :: Int -> Int
substringsForLength n = (n * (n + 1)) `div` 2

substringsForLength1000 = substringsForLength 1000

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

slicesToSet :: String -> [(Int, Int)] -> Set (Vector Char)
slicesToSet str sliceInstructions = 
  Prelude.foldr f S.empty sliceInstructions
  
  where 
    f :: (Int, Int) -> Set (Vector Char) -> Set (Vector Char)
    f (start, run) acc = S.insert (V.slice start run vstr) acc

    vstr :: Vector Char
    vstr = V.fromList str

drawShrinkingSet :: [String] -> Set (Vector Char) -> [Vector Char]
drawShrinkingSet dnas startSet = S.toList $ Prelude.foldr f startSet dnas
  where 
    f :: String -> Set (Vector Char) -> Set (Vector Char)
    f dna prevSet = S.intersection prevSet thisSet
      where 
        filteredSlices :: [(Int, Int)]
        filteredSlices = lengthToStartRunList $ lengthLongest prevSet
        
        thisSet :: Set (Vector Char)
        thisSet = slicesToSet dna filteredSlices
        
        lengthLongest :: Set (Vector Char) -> Int
        lengthLongest = Prelude.length . L.maximumBy (compare `on` Prelude.length) . S.toList 
        
fileName = "/Users/brodyberg/Documents/GitHub/Notes/ProjectRosalind.hsproj/LearnHaskell/FindingASharedMotif/rosalind_lcsm_2.txt"

mainToSet :: IO ()
mainToSet = do
    now <- getZonedTime  

    putStrLn "START: just one " 
    putStrLn $ show now

    fastas <- filePathToFastas fileName

    putStrLn "fasta count: " 
    putStrLn $ show $ Prelude.length fastas

    now <- getZonedTime  
    putStrLn "START: all substrings on 2"
    putStrLn $ show now

--    let twoFastas = L.take 2 fastas
    let dnas = fmap fastaSeq fastas

    let allSubs1 = slicesToSet (dnas !! 0) startRunListForLength1000
    let allSubs2 = slicesToSet (dnas !! 1) startRunListForLength1000

    putStrLn "size 1: "
    putStrLn $ show $ S.size allSubs1    


    putStrLn "size 2: "
    putStrLn $ show $ S.size allSubs2
  
    now <- getZonedTime    
    putStrLn "START intersection of 2"
    putStrLn $ show now
        
    let isection = S.intersection allSubs1 allSubs2

    now <- getZonedTime    
    putStrLn "END intersection of 2"
    putStrLn $ show now

  
    putStrLn "Intersection size: " 
    
    let size = S.size isection
    putStrLn $ show size

    now <- getZonedTime  
    putStrLn "END: all substrings on 2"
    putStrLn $ show now
  
    now <- getZonedTime    
    putStrLn "START get max item"
    putStrLn $ show now

    -- of intersection what is the longest string?

    let sMax = S.findMax isection
    
    putStrLn "max item: " 
    putStrLn $ show sMax


    now <- getZonedTime    
    putStrLn "END get max item"
    putStrLn $ show now

    now <- getZonedTime    
    putStrLn "START toList: " 
    putStrLn $ show now

--    let top10 = L.take 10 $ S.toDescList isection
    let lst = S.toList isection
    let top = L.maximumBy (compare `on` Prelude.length) lst

    putStrLn "top: " 
    putStrLn $ show top
      
    now <- getZonedTime    
    putStrLn "END toList: " 
    putStrLn $ show now
    
    now <- getZonedTime    
    putStrLn "START draw 2: " 
    putStrLn $ show now

    let result = drawShrinkingSet (L.drop 96 dnas) isection
    
    putStrLn "result size: " 
    putStrLn $ show $ Prelude.length result

    now <- getZonedTime    
    putStrLn "END draw 2: " 
    putStrLn $ show now
  




-- property: 
-- if we filter a list to all runs <= given run
-- that list is == in length to the theoretical number
--   of substrings for a list of the given run length
-- honestly, who cares, we can just call 
-- lengthToStartRunList $ length of longest item 


























    putStrLn "Done"
