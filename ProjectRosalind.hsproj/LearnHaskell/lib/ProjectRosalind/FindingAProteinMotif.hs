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

---ids = ["A2Z669", "B5ZC00", "P07204_TRBM_HUMAN", "P20840_SAG1_YEAST"]

--ids = ["Q05865", "A5A3H2", "P12630_BAR1_YEAST", "B4S2L7", "Q4FZD7", "Q8CE94", "P02725_GLP_PIG", "P01045_KNH2_BOVIN", "Q05557", "P31096_OSTP_BOVIN", "P80195_MPP3_BOVIN", "P01876_ALC1_HUMAN", "P11279_LMP1_HUMAN", "Q3T0C9"] 

--ids = ["P09136", "P0AF66", "P01046_KNL1_BOVIN", "P36913_EBA3_FLAME", "Q5WFN0", "P28653_PGS1_MOUSE", "P72173", "P17967", "P04441_HG2A_MOUSE", "P04921_GLPC_HUMAN", "P37803", "Q0SU18", "P01878_ALC_MOUSE"]

ids = ["P02725_GLP_PIG", "P39873_RNBR_BOVIN", "B9LIC8", "P36912_EBA2_FLAME", "P11171_41_HUMAN", "O14977", "P21810_PGS1_HUMAN", "P12763_A2HS_BOVIN", "Q8P5E4", "Q16775", "P20840_SAG1_YEAST", "B5ZC00", "P19835_BAL_HUMAN", "P08198_CSG_HALHA", "P19827_ITH1_HUMAN"]

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

parseFasta :: String -> String -> String -> Fasta
parseFasta s regex givenId = (eToV . parse fasta "error") s
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
        
        findLocations :: String -> [Int]
        -- hack + 1 is because Project Rosalind is using 1-indexes
        findLocations fs = map (\(ix, _) -> ix + 1) matches
          where 
            matches = getAllMatches (fs =~ regex)  :: [(Int, Int)]

-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html
main = do
    contents <- urlsToContent' $ idsToFastaUrl ids
    putStr $ toString $ resultsWithHits contents ids
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

      resultsWithHits :: [String] -> [String] -> [Fasta]
      resultsWithHits contents givenIdentifiers = filter hasHits (results contents givenIdentifiers)
        where
          -- https://stackoverflow.com/questions/38052553/haskell-record-pattern-matching
          hasHits :: Fasta -> Bool
          hasHits Fasta { locations = ls } = length ls > 0

          results :: [String] -> [String] -> [Fasta]
          results content givenIdentifiers = map fastaFromContent $ zip content givenIdentifiers

          fastaFromContent :: (String, String) -> Fasta
          fastaFromContent (content, givenId) = parseFasta content glycosylationRegex givenId

          glycosylationRegex :: String
          glycosylationRegex = "[N][^P](S|T)[^P]"