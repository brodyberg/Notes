module ProjectRosalind.FSMSlicesVector where
  
import Data.Vector as V
import Data.Set as S
import Data.List as L

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

startRunListForLength1000 = lengthToStartRunList 1000

substringsForLength :: Int -> Int
substringsForLength n = (n * (n + 1)) `div` 2

substringsForLength1000 = substringsForLength 1000

slicesSet :: String -> Set (Vector Char)
slicesSet str = vectorToSet $ generateVector len f
  where 
    vectorToSet :: Vector (Vector Char) -> Set (Vector Char)
    vectorToSet = V.foldr (\i acc -> S.insert i acc) S.empty
    
    generateVector :: Int -> (Int -> (Vector Char)) -> Vector (Vector Char)
    --                              FILTER
    generateVector len = V.generate substringsForLength1000-- (substringsForLength len)

    f :: Int -> Vector Char
    f ix = V.slice start run vstr
      where
                       -- USE CHAINED FILTERED
        (start, run) = startRunListForLength1000 !! ix

    len :: Int    
    len = Prelude.length str
    
    vstr :: Vector Char
    vstr = V.fromList str

-- why not iterate across substringsForLength1000 straight into Set?
--slicesStraightToSet

slicesVector :: String -> Vector (Vector Char)
slicesVector str = generateVector len f
  where 
    generateVector :: Int -> (Int -> (Vector Char)) -> Vector (Vector Char)
    --                              FILTER
    generateVector len = V.generate substringsForLength1000-- (substringsForLength len)

    f :: Int -> Vector Char
    f ix = V.slice start run vstr
      where
                       -- USE CHAINED FILTERED
        (start, run) = startRunListForLength1000 !! ix

    len :: Int    
    len = Prelude.length str
    
    vstr :: Vector Char
    vstr = V.fromList str


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

mainSlices :: IO ()
mainSlices = do
    now <- getZonedTime  

    

    putStrLn "START: just one " 
    putStrLn $ show now

    fastas <- filePathToFastas fileName

    putStrLn "fasta count: " 
    putStrLn $ show $ Prelude.length fastas

    now <- getZonedTime  
    putStrLn "START: all substrings on two"
    putStrLn $ show now

    let twoFastas = L.take 2 fastas
    let twoDnas = fmap fastaSeq twoFastas

--    let allSubs1 = slicesSet $ twoDnas !! 0
--    let allSubs2 = slicesSet $ twoDnas !! 1
    let allSubs1 = slicesVector $ twoDnas !! 0
    let allSubs2 = slicesVector $ twoDnas !! 1
    
--    let isection = S.intersection allSubs1 allSubs2

    let length1 = V.length allSubs1
    let length2 = V.length allSubs2

--    let size = S.size isection
--    let tSize = theoreticalSubstringCount dna
--    let tSize = theoreticalSize dna

    putStrLn "size 1: "
    putStrLn $ show length1
--    putStrLn $ show $ S.size allSubs1    


    putStrLn "size 2: "
    putStrLn $ show length2
--    putStrLn $ show $ S.size allSubs2
  
--    putStrLn "Count savings: " 
--    putStrLn $ show (size - tSize)

--    putStrLn $ show allSubs
  
    putStrLn "Intersection size: " 
    putStrLn $ show size

--    putStrLn $ show $ size allSubs

    now <- getZonedTime  
    putStrLn "END: all substrings on two"
    putStrLn $ show now
    
--    putStrLn "START: Making list of all fastas" 
--    now <- getZonedTime
--    putStrLn $ show now
--   
--    let fastaList = fmap fastaSeq fastas
--    now <- getZonedTime
--    putStrLn "DONE: Making list of all fastas" 
--    putStrLn $ show now
--
--    now <- getZonedTime
--    putStrLn "START: allSubstrings on all fastas" 
--    putStrLn $ show now
--
--    let resultsAsListOfLists = fmap allSubstrings fastaList
--
--    now <- getZonedTime
--    putStrLn "END: allSubstrings on all fastas" 
--    putStrLn $ show now

   
    putStrLn "Done"
