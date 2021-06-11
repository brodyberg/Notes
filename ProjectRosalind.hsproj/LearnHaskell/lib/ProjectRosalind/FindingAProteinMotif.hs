module ProjectRosalind.FindingAProteinMotif where

import qualified Data.ByteString.Lazy.Char8 as L8

import Text.Parsec
import Text.Parsec.String (Parser)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Map as Map
import qualified System.IO as IO

import Text.Regex.TDFA (getAllMatches, (=~))

import ProjectRosalind.Fasta (eoe, eol)

import Data.List.Split (splitOn)
import Control.Monad (mapM)

-- http://rosalind.info/problems/mprt/
-- https://www.uniprot.org/uniprot/B5ZC00.fasta
-- https://www.uniprot.org/uniprot/A2Z669.fasta
-- https://www.uniprot.org/uniprot/P07204.fasta
-- https://www.uniprot.org/uniprot/P20840_SAG1_YEAST.fasta
-- https://www.uniprot.org/uniprot/P07204_TRBM_HUMAN.fasta

ids = ["A2Z669", "B5ZC00", "P07204_TRBM_HUMAN", "P20840_SAG1_YEAST"]

-- Algebraic
data NGlycosylationMotif = NGlycosylationMotif { name :: String
                                   , fastaSeq     :: String
                                   , locations    :: [Int]
                                   } deriving (Eq, Ord, Show)

-- Classes
class ShowNGlycosylationMotif a where
    showResults :: a -> String

-- Instances
instance ShowNGlycosylationMotif NGlycosylationMotif where
    showResults NGlycosylationMotif { name = x, locations = xs } = 
      concat [ x
             , "\n"
             , foldr (++) "" $ fmap (\n -> (show n) ++ " ") xs
             , "\n" ]

parseFasta :: String -> String -> NGlycosylationMotif
parseFasta s regex = (eToV . parse fasta "error") s
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

    fasta :: Parsec String u NGlycosylationMotif
    fasta = do
      spaces
      char '>'
      header <- manyTill (satisfy (/= '>')) eol
      fseq <- manyTill anyChar eoe
      return ( NGlycosylationMotif { 
        name = (justTheName header)
      , fastaSeq = makeProteinList fseq 
      , locations = findLocations $ makeProteinList fseq } )
      where
        -- type
        removeWhitespace = filter (`notElem` "\n\r ")
    
        justTheName :: String -> String
        justTheName header = (splitOn "|" $ header) !! 1
        
        makeProteinList :: String -> String
        makeProteinList = removeWhitespace
        
        findLocations :: String -> [Int]
        -- hack + 1 is because Project Rosalind is using 1-indexes
        findLocations fs = map (\(ix, _) -> ix + 1) matches
          where 
            regex = "[N][^P](S|T)[^P]"
            matches = getAllMatches (fs =~ regex)  :: [(Int, Int)]


-- we get list of ids
-- make list of urls
-- read from net
-- read text into fasta
-- search fasta text
-- retrieve list of docs
-- read each doc for match string indexes
-- print name and indexes

idsToFastaUrl :: [String] -> [String]
idsToFastaUrl = map (\s -> urlBase ++ s ++ urlExt)
  where 
    urlBase = "https://www.uniprot.org/uniprot/" -- B5ZC00.fasta
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

resultsWithHits :: [String] -> [NGlycosylationMotif]
resultsWithHits contents = filter hasHits (results contents)
  where
    -- https://stackoverflow.com/questions/38052553/haskell-record-pattern-matching
    hasHits :: NGlycosylationMotif -> Bool
    hasHits NGlycosylationMotif { locations = ls } = length ls > 0

    results :: [String] -> [NGlycosylationMotif]
    results = map fastaFromContent 

    fastaFromContent :: String -> NGlycosylationMotif
    fastaFromContent s = parseFasta s glycosylationRegex

    glycosylationRegex :: String
    glycosylationRegex = "[N][^P](S|T)[^P]"

-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html
main = do
    contents <- urlsToContent' $ idsToFastaUrl ids
    putStr $ toString $ resultsWithHits contents    
    where 
      toString :: [NGlycosylationMotif] -> String
      toString = foldr (\g acc -> (showResults g) ++ acc) ""