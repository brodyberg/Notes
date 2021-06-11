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
data Fasta = Fasta { name      :: String
                   , fastaSeq  :: String
                   , locations :: [Int] } deriving (Eq, Ord, Show)

-- Classes
class ShowFasta a where
    showResults :: a -> String

-- Instances
instance ShowFasta Fasta where
    showResults Fasta { name = x, locations = xs } = 
      concat [ x
             , "\n"
             , foldr (++) "" $ fmap (\n -> (show n) ++ " ") xs
             , "\n" ]

parseFasta :: String -> String -> Fasta
parseFasta s regex = (eToV . parse fasta "error") s
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
            matches = getAllMatches (fs =~ regex)  :: [(Int, Int)]

-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html
main = do
    contents <- urlsToContent' $ idsToFastaUrl ids
    putStr $ toString $ resultsWithHits contents    
    where 
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

      resultsWithHits :: [String] -> [Fasta]
      resultsWithHits contents = filter hasHits (results contents)
        where
          -- https://stackoverflow.com/questions/38052553/haskell-record-pattern-matching
          hasHits :: Fasta -> Bool
          hasHits Fasta { locations = ls } = length ls > 0

          results :: [String] -> [Fasta]
          results = map fastaFromContent 

          fastaFromContent :: String -> Fasta
          fastaFromContent s = parseFasta s glycosylationRegex

          glycosylationRegex :: String
          glycosylationRegex = "[N][^P](S|T)[^P]"