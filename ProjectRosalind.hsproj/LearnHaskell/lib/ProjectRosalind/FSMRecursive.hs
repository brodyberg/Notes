module ProjectRosalind.FSMRecursive where
  
import Data.Set as S
import Data.Vector as V
import Data.List as L

import ProjectRosalind.Fasta (parseFasta')
import ProjectRosalind.Fasta_Types 
import System.IO (openFile, hGetContents, IOMode(ReadMode))
import Control.Monad

import Data.Time

-- Carry string 
-- Copy of carry (block)
-- Next is that without last item
-- Continue while there’s any more of copy 
-- None left? Knock first from carry, loop

build :: String -> Set (V.Vector Char)
build str = create (V.fromList str) (V.fromList str) S.empty
  where 
    create :: (Vector Char) -> (Vector Char) -> Set (Vector Char) -> Set (Vector Char)
    create carry block acc = 
      -- No more carry
      -- No more block:
      --   1. return acc
      if carryLen == 0 && blockLen == 0
      then
        acc
      -- There is more carry
      -- No more of this block:
      --   1. bump one off head of carry
      --   2. new carry value placed as new block
      --   3. insert that carry value into acc
      else if blockLen == 0
      then 
        create (V.tail carry) (V.tail carry) acc
      -- There is more carry
      -- There is more of this block: 
      --   1. do not touch carry
      --   2. bump one off tail of block
      --   3. save block to acc
      else
        create carry (V.init block) (S.insert block acc)
        
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
  
--prop_allPossibleSubstringCount :: String -> Bool
--prop_allPossibleSubstringCount str = 
--  length (allSubstrings str) == (n * (n + 1)) `div` 2
--  where n = length str

theoreticalSubstringCount :: String -> Int 
theoreticalSubstringCount s = (n * (n + 1)) `div` 2
  where n = Prelude.length s


mainBuild :: IO ()
mainBuild = do
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

    let allSubs1 = build $ twoDnas !! 0
    let allSubs2 = build $ twoDnas !! 1
    
    let isection = S.intersection allSubs1 allSubs2

    let size = S.size isection
--    let tSize = theoreticalSubstringCount dna
--    let tSize = theoreticalSize dna

    putStrLn "size 1: "
    putStrLn $ show $ S.size allSubs1    


    putStrLn "size 2: "
    putStrLn $ show $ S.size allSubs2
  
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