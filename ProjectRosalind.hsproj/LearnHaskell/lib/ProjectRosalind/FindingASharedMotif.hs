module ProjectRosalind.FindingASharedMotif where

import Test.QuickCheck
  
import Data.Vector as V

import ProjectRosalind.Fasta (parseFasta')
import ProjectRosalind.Fasta_Types 
import System.IO (openFile, hGetContents, IOMode(ReadMode))
import Control.Monad

import Data.Time

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
  
mainBuild :: IO ()
mainBuild = do
    now <- getZonedTime  

    putStrLn "START: just one " 
    putStrLn $ show now

    fastas <- filePathToFastas fileName

    putStrLn "fasta count: " 
    putStrLn $ show $ Prelude.length fastas

    now <- getZonedTime  
    putStrLn "START: all substrings on one"
    putStrLn $ show now

    let first = fastas !! 0
    let dna = fastaSeq first

    let allSubs = build dna
    
    putStrLn $ show allSubs

    now <- getZonedTime  
    putStrLn "END: all substrings on one"
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
