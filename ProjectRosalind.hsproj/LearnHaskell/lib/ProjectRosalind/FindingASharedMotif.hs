module ProjectRosalind.FindingASharedMotif where
  
import Data.Set as S
import Data.Vector as V

import ProjectRosalind.Fasta (parseFasta')
import ProjectRosalind.Fasta_Types 
import System.IO (openFile, hGetContents, IOMode(ReadMode))
import Control.Monad

import Data.Time

slices :: String -> V.Vector (V.Vector Char)
slices str = V.generate (substringsForLength len) f
  where 
    f :: Int -> (V.Vector Char)
    f ix = V.slice start run vstr
      where
        (start, run) = startRunList !! ix

    len :: Int    
    len = Prelude.length str
    
    startRunList :: [(Int, Int)]
    startRunList = lengthToStartRunList len
    
    vstr :: (V.Vector Char)
    vstr = V.fromList str
    
    substringsForLength :: Int -> Int
    substringsForLength n = (n * (n + 1)) `div` 2

    lengthToStartRunList :: Int -> [(Int, Int)]
    lengthToStartRunList len = 
      Prelude.concat 
        [[ (l, (r - l) + 1) 
          | l <- [0..(len - 1)], l <= r ] 
          | r <- [(len - 1), (len - 2)..0] ]

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

    -- do the full thing, time it

--    putStrLn "START: just one " 
--    putStrLn $ show now
--
--    fastas <- filePathToFastas fileName
--
--    putStrLn "fasta count: " 
--    putStrLn $ show $ Prelude.length fastas
--
--    now <- getZonedTime  
--    putStrLn "START: all substrings on two"
--    putStrLn $ show now
--
--    let twoFastas = L.take 2 fastas
--    let twoDnas = fmap fastaSeq twoFastas
--
--    let allSubs1 = build $ twoDnas !! 0
--    let allSubs2 = build $ twoDnas !! 1
--    
--    let isection = S.intersection allSubs1 allSubs2
--
--    let size = S.size isection
----    let tSize = theoreticalSubstringCount dna
----    let tSize = theoreticalSize dna
--
--    putStrLn "size 1: "
--    putStrLn $ show $ S.size allSubs1    
--
--
--    putStrLn "size 2: "
--    putStrLn $ show $ S.size allSubs2
--  
----    putStrLn "Count savings: " 
----    putStrLn $ show (size - tSize)
--
----    putStrLn $ show allSubs
--  
--    putStrLn "Intersection size: " 
--    putStrLn $ show size
--
----    putStrLn $ show $ size allSubs
--
--    now <- getZonedTime  
--    putStrLn "END: all substrings on two"
--    putStrLn $ show now
    
    putStrLn "START: Making list of all fastas" 
    now <- getZonedTime
    putStrLn $ show now
   
    fastas <- filePathToFastas fileName
    putStrLn "length: "
    putStrLn $ show $ Prelude.length fastas

    now <- getZonedTime
    putStrLn "DONE: Making list of 10 fastas" 
    putStrLn $ show now
    
    let dnas = Prelude.take 10 $ fmap fastaSeq fastas
--    let dnas = fmap fastaSeq fastas

    putStrLn $ show $ fmap Prelude.length dnas

    now <- getZonedTime
    putStrLn "START: allSubstrings on 10 fastas" 
    putStrLn $ show now
    
    let vectors = fmap slices dnas

    let xcount = V.length $ vectors !! 0

    putStrLn "count of substrings from first: " 
    putStrLn $ show xcount
    
    putStrLn "first substring of each: " 
  
    now <- getZonedTime
    putStrLn "END: allSubstrings on 10 fastas" 
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
