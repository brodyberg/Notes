module ProjectRosalind.FindingASharedMotif where

import Test.QuickCheck
import Data.List (intersect, maximumBy)
import Data.Function (on)

import ProjectRosalind.Fasta (parseFasta')
import ProjectRosalind.Fasta_Types 
import System.IO  
import Control.Monad

allSubstrings :: String -> [String]
allSubstrings [] = []
allSubstrings strings = allSubstrings' strings []
  where 
    allSubstrings' [] acc = acc
    allSubstrings' str acc = (substrings str) ++ (allSubstrings' (tail str) [])

substrings :: String -> [String]
substrings []   = []
substrings item = ss "" item []
  where 
    ss [] [] acc = acc
    ss [] therest acc = ss [(head therest)] (tail therest) acc
    ss save [] acc = save : acc 
    ss save therest acc = ss (save ++ [(head therest)]) (tail therest) (save : acc)

getLongestSubstring :: [String] -> String
getLongestSubstring strings = 
  maximumBy (compare `on` length) resultsOfIntersections
  where 
    resultsAsListOfLists = fmap allSubstrings strings
    resultsOfIntersections = foldr intersect (head resultsAsListOfLists) (tail resultsAsListOfLists)

-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_allPossibleSubstringCount :: String -> Bool
prop_allPossibleSubstringCount str = 
  length (allSubstrings str) == (n * (n + 1)) `div` 2
  where n = length str

fileName = "/Users/brodyberg/Documents/GitHub/Notes/ProjectRosalind.hsproj/LearnHaskell/FindingASharedMotif/rosalind_lcsm_1.txt"

readLocalFile :: String -> IO String
readLocalFile path = do  
        handle <- openFile path ReadMode
        contents <- hGetContents handle
        return contents

filePathToFastas :: String -> IO [FastaSequence]
filePathToFastas path = do
  contents <- readLocalFile path
  return $ parseFasta' contents
  
mainSubstrings :: IO ()
mainSubstrings = do
    fastas <- filePathToFastas fileName

    let longest = getLongestSubstring $ fmap fastaSeq fastas

    putStrLn longest
