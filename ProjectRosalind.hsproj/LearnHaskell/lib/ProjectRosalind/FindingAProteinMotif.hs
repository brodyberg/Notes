module ProjectRosalind.FindingAProteinMotif where

import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Parsec
import Text.Parsec.String (Parser)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Map as Map
import qualified System.IO as IO
import Text.Regex.TDFA (getAllMatches, getAllTextMatches, (=~))
import ProjectRosalind.Fasta (eoe, eol)
import Data.List.Split (splitOn)
import Data.String (lines)
import Control.Monad (mapM)

import Test.QuickCheck

-- clearly we need to
-- read a file
-- each line is an id
-- do the rest of the algorithm

import System.IO  
import Control.Monad

readLocalFile :: String -> IO String
readLocalFile path = do  
        handle <- openFile path ReadMode
        contents <- hGetContents handle
        return contents

filepathToIds :: String -> IO [String]
filepathToIds path = do
  contents <- readLocalFile path
  return $ lines contents
  
-- Algebraic
data Fasta = Fasta { givenId   :: String
                   , name      :: String
                   , fastaSeq  :: String
                   , locations :: [Int] } deriving (Eq, Ord, Show)

-- Classes
class ShowFasta a where
    showResults :: a -> String

-- Instances
instance ShowFasta Fasta where
    showResults Fasta { givenId = x, locations = xs } = 
      concat [ x
             , "\n"
             , foldr (++) "" $ fmap (\n -> (show n) ++ " ") xs
             , "\n" ]

parseFasta :: String -> (String -> [Int]) -> String -> Fasta
parseFasta s findLocations givenId = (eToV . parse fasta "error") s
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

    fasta :: Parsec String u Fasta
    fasta = do
      spaces
      char '>'
      header <- manyTill (satisfy (/= '>')) eol
      fseq <- manyTill anyChar eoe
      return ( Fasta { 
        givenId = givenId 
      , name = (justTheName header)
      , fastaSeq = makeProteinList fseq 
      , locations = findLocations $ makeProteinList fseq } )
      where
        -- type
        removeWhitespace = filter (`notElem` "\n\r ")
    
        justTheName :: String -> String
        justTheName header = (splitOn "|" $ header) !! 1
        
        makeProteinList :: String -> String
        makeProteinList = removeWhitespace
        
-- https://stackoverflow.com/questions/38052553/haskell-record-pattern-matching
hasHits :: Fasta -> Bool
hasHits Fasta { locations = ls } = length ls > 0

results :: [String] -> [String] -> (String -> [Int]) -> [Fasta]
results content givenIdentifiers findLocations = map (fastaFromContent findLocations) $ zip content givenIdentifiers

fastaFromContent :: (String -> [Int]) -> (String, String) -> Fasta
fastaFromContent findLocations (content, givenId) = parseFasta content findLocations givenId

toString :: [Fasta] -> String
toString = foldr (\g acc -> (showResults g) ++ acc) ""
      
idsToFastaUrl :: [String] -> [String]
idsToFastaUrl = map (\s -> urlBase ++ s ++ urlExt)
  where 
    urlBase = "https://www.uniprot.org/uniprot/"
    urlExt  = ".fasta"

urlsToContent' :: [String] -> IO [String]
urlsToContent' urls = do 
  mapM pullFromWeb urls
  where
    pullFromWeb :: String -> IO String
    pullFromWeb s = do
      manager <- newManager tlsManagerSettings
      request <- parseRequest s
      response <- httpLbs request manager  
      return $ L8.unpack $ responseBody response

mainSubstrings = do 
    ids <- filepathToIds filePath

    contents <- urlsToContent' $ idsToFastaUrl ids
    putStr $ toString $ resultsWithHits contents ids

    where 
      resultsWithHits :: [String] -> [String] -> [Fasta]
      resultsWithHits contents givenIdentifiers = filter hasHits (results contents givenIdentifiers findLocations)
      findLocations :: String -> [Int]
      -- hack + 1 is because Project Rosalind is using 1-indexes
      findLocations fs = map (\(ix, _) -> ix + 1) matches
        where 
          possibleMatches = filter (\(ix, str) -> length str == 4) $ allSubstrings fs
          matches = filter (\(ix, str) -> str =~ glycosylationRegex) possibleMatches

glycosylationRegex :: String
glycosylationRegex = "[N][^P](S|T)[^P]"

allSubstrings :: String -> [(Int, String)]
allSubstrings [] = []
allSubstrings strings = allSubstrings' 0 strings []
  where 
    allSubstrings' :: Int -> String -> [(Int, String)] -> [(Int, String)]
    allSubstrings'    _      []        acc = acc
    allSubstrings'    ix     str       acc = (substringsIx ix str) ++ (allSubstrings' (ix + 1) (tail str) [])      

    substringsIx :: Int -> String -> [(Int, String)]
    substringsIx    _      []      = []
    substringsIx    ix     item    = ss ix (0, "") item []
      where 
        ss :: Int -> (Int, String) -> String -> [(Int, String)] -> [(Int, String)]
        -- save and the rest are empty
        ss    ix     (_,"")           []        acc              = acc
        -- save is empty
        ss    ix     (_,"")           therest   acc = ss ix (ix, [(head therest)]) (tail therest) acc
        -- the rest is empty
        ss    ix     save             []        acc = save : acc 
        ss    ix     (ix', save)      therest   acc = ss ix (ix, save ++ [(head therest)]) (tail therest) ((ix', save) : acc)

-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_allPossibleSubstringCount :: String -> Bool
prop_allPossibleSubstringCount str = 
  length (allSubstrings str) == (n * (n + 1)) `div` 2
  where n = length str

filePath = "/Users/brodyberg/Documents/GitHub/Notes/ProjectRosalind.hsproj/LearnHaskell/FindingAMotif/rosalind_mprt_4.txt"

-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html
mainREGEX :: IO ()
mainREGEX = do
    ids <- filepathToIds filePath

    contents <- urlsToContent' $ idsToFastaUrl ids
    putStr $ toString $ resultsWithHits contents ids
    -- putStr $ toString $ resultsWithHits contents ids    

    where 
      resultsWithHits :: [String] -> [String] -> [Fasta]
      resultsWithHits contents givenIdentifiers = filter hasHits (results contents givenIdentifiers findLocations)
        where          
          findLocations :: String -> [Int]
          -- hack + 1 is because Project Rosalind is using 1-indexes
          findLocations fs = map (\(ix, _) -> ix + 1) matches
            where 
              matches = getAllMatches (fs =~ glycosylationRegex)  :: [(Int, Int)]