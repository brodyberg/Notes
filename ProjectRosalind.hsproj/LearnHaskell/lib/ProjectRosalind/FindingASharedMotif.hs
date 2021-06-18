module ProjectRosalind.FindingASharedMotif where

import Test.QuickCheck
import Data.List (intersect, maximumBy, take)
import Data.Function (on)

import ProjectRosalind.Fasta (parseFasta')
import ProjectRosalind.Fasta_Types 
import System.IO  
import Control.Monad

import Text.Regex.TDFA ((=~))

import Data.Time

-- speed ideas: 
-- do the usual common substring approach for the first n
-- but then, convert over to a simple search using those

-- take first
-- make all common substrings
-- continually search each for the previously matched
--   substrings
-- but after the first string, you don't make all common substrings
--   again

allSubstrings :: String -> [String]
allSubstrings [] = []
allSubstrings strings = allSubstrings' strings []
  where 
    allSubstrings' [] acc = acc
    allSubstrings' str acc = (substrings str) ++ (allSubstrings' (tail str) [])

-- do everything here: https://wiki.haskell.org/Dynamic_programming_example

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
    resultsAsListOfLists :: [[String]]
    resultsAsListOfLists = fmap allSubstrings strings

    resultsOfIntersections = foldr intersect (head resultsAsListOfLists) (tail resultsAsListOfLists)

-- longest run: 5:20

-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_allPossibleSubstringCount :: String -> Bool
prop_allPossibleSubstringCount str = 
  length (allSubstrings str) == (n * (n + 1)) `div` 2
  where n = length str

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
  
mainSubstrings :: IO ()
mainSubstrings = do
    now <- getZonedTime  

    putStrLn "START: prestart " 
    putStrLn $ show now

    fastas <- filePathToFastas fileName

    now <- getZonedTime  
    putStrLn "START: all substrings on one"
    putStrLn $ show now

    let first = fastas !! 0
    let dna = fastaSeq first

    let allSubs = allSubstrings dna
    
    putStrLn $ show allSubs

    now <- getZonedTime  
    putStrLn "END: all substrings on one"
    putStrLn $ show now
    
    putStrLn "START: Making list of all fastas" 
    now <- getZonedTime
    putStrLn $ show now
   
    let fastaList = fmap fastaSeq fastas
    now <- getZonedTime
    putStrLn "DONE: Making list of all fastas" 
    putStrLn $ show now

    now <- getZonedTime
    putStrLn "START: allSubstrings on all fastas" 
    putStrLn $ show now

    let resultsAsListOfLists = fmap allSubstrings fastaList

    now <- getZonedTime
    putStrLn "END: allSubstrings on all fastas" 
    putStrLn $ show now

   
    let no = foldr intersect (head resultsAsListOfLists) []
    
    putStrLn "foo"
    
    let shortList = take 2 resultsAsListOfLists

    let nope = foldr intersect (head shortList) (tail shortList)

    putStrLn $ show nope
    
    let bar = maximumBy (compare `on` length) nope
   
    putStrLn "bar"

    -- ok, so this forced computation that wasn't otherwise happening
    putStrLn bar
    
    -- put all substrings into a dictionary where the substring is the key
    -- but what's the value? how to check it's the substring of all fasta?
 
--    putStrLn "START: getLongestSubstring 2 fastas" 
--    now <- getZonedTime
--    putStrLn $ show now
--   
--    let longest = getLongestSubstring fastaList
--
--    putStrLn longest
--
--    now <- getZonedTime
--    putStrLn "END: getLongestSubstring 2 fastas" 
--    putStrLn $ show now

    putStrLn "Done"

    
--
--    let longest = getLongestSubstring $ fmap fastaSeq fastas
--
--    putStrLn longest
--
--    end <- getZonedTime
--
--    putStrLn "end: "     
--    putStrLn $ show end
    

