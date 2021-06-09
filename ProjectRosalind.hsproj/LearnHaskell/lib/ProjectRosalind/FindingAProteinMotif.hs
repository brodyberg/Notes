module ProjectRosalind.FindingAProteinMotif where

import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Map as Map
import qualified System.IO as IO

import Text.Regex.TDFA (getAllMatches, (=~))

--xn = getAllMatches ("john anne yifan" =~ "[a-z]+") :: [(Int, Int)]

-- [N][^P](S|T)[^P]

nnn = "MKNKFKTQEELVNHLKTVGFVFANSEIYNGLANAWDYGPLGVLLKNNLKNLWWKEFVTKQ\
\KDVVGLDSAIILNPLVWKASGHLDNFSDPLIDCKNCKARYRADKLIESFDENIHIAENSS\
\NEEFAKVLNDYEISCPTCKQFNWTEIRHFNLMFKTYQGVIEDAKNVVYLRPETAQGIFVN\
\FKNVQRSMRLHLPFGIAQIGKSFRNEITPGNFIFRTREFEQMEIEFFLKEESAYDIFDKY\
\LNQIENWLVSACGLSLNNLRKHEHPKEELSHYSKKTIDFEYNFLHGFSELYGIAYRTNYD\
\LSVHMNLSKKDLTYFDEQTKEKYVPHVIEPSVGVERLLYAILTEATFIEKLENDDERILM\
\DLKYDLAPYKIAVMPLVNKLKDKAEEIYGKILDLNISATFDNSGSIGKRYRRQDAIGTIY\
\CLTIDFDSLDDQQDPSFTIRERNSMAQKRIKLSELPLYLNQKAHEDFQRQCQK"

-- https://github.com/haskell-hvr/regex-tdfa
qqqn = getAllMatches (nnn =~ "[N][^P](S|T)[^P]")  :: [(Int, Int)]


-- http://rosalind.info/problems/mprt/
  
-- make a dfa that recognizes a DSL for motifs
-- we could easily take the Fasta lib and learn from 
-- it to author a parser for this and then run it
-- on the output of http requests given the 
-- UniProt Protein Database access IDs.

-- https://www.uniprot.org/uniprot/B5ZC00.fasta
-- https://www.uniprot.org/uniprot/A2Z669.fasta
-- https://www.uniprot.org/uniprot/P07204.fasta
-- 

-- https://stackoverflow.com/questions/36078405/parsec-getting-start-and-end-source-positions-of-expressions

-- Algebraic
data NGlycosylationMotif = NGlycosylationMotif { proteinId :: String
                                   , locations    :: [Int]
                                   } deriving (Eq, Ord, Show)

a = NGlycosylationMotif { proteinId = "AB5ZC00"
                        , locations = [85, 118, 142, 306, 395] } 

-- Classes
class ShowNGlycosylationMotif a where
    showResults :: a -> String

-- Instances
instance ShowNGlycosylationMotif NGlycosylationMotif where
    showResults NGlycosylationMotif {proteinId = x, locations = xs} = 
      concat [ x
             , "\n"
             , foldr (++) "" $ fmap (\n -> (show n) ++ " ") xs ]

ids = ["A2Z669", "B5ZC00", "P07204_TRBM_HUMAN", "P20840_SAG1_YEAST"]

-- we get list of ids
-- make list of urls
-- read from net
-- read text into fasta
-- search fasta text
-- retrieve list of docs
-- read each doc for match string indexes
-- print name and indexes
urlBase = "https://www.uniprot.org/uniprot/" -- B5ZC00.fasta
urlExt  = ".fasta"

idsToFastaUrl :: [String] -> [String]
idsToFastaUrl = map (\s -> urlBase ++ s ++ urlExt)

urlsActual = idsToFastaUrl ids

pullFastaFromWeb :: String -> IO String
pullFastaFromWeb s = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest s
  response <- httpLbs request manager  
  pure $ L8.unpack $ responseBody response
 



 
main = do
    manager <- newManager tlsManagerSettings

    payloads <- map (\s -> 
                            do 
                              request <- parseRequest s
                              response <- httpLbs request manager  
                              L8.unpack $ responseBody response) urlsActual
    
    putStr "foo"

--    request <- parseRequest "https://www.uniprot.org/uniprot/P07204.fasta"
--    response <- httpLbs request manager  
--    
--    let x = L8.unpack $ responseBody response

--    putStr x
    
--    putStr $ L8.unpack response

--    L8.putStrLn $ responseBody response
    
--`N{P}[ST]{P}`. Or, the N amino acid, any amino acid except P followed by either S or T amino acids and finally any amino acid except P. 

-- Parser: 
-- N, any but P, either S or T, and finally any but P
-- Combinators: 
--  N
--  any but P
--  S or T
-- Combined such that: 
--  N
--  any but P
--  S or T
--  any but P





























