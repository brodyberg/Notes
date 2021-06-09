module ProjectRosalind.FindingAProteinMotif where

import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Map as Map
import qualified System.IO as IO

import Text.Regex.TDFA (getAllMatches, (=~))

import ProjectRosalind.Fasta (parseFasta)
import ProjectRosalind.Fasta_Types (FastaSequence, fastaSeq, fastaHeader)

import Data.List.Split (splitOn)

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

fullEx = urlBase ++ "B5ZC00" ++ urlExt

idsToFastaUrl :: [String] -> [String]
idsToFastaUrl = map (\s -> urlBase ++ s ++ urlExt)

urlsActual = idsToFastaUrl ids

pullFromWeb :: String -> IO String
pullFromWeb s = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest s
  response <- httpLbs request manager  
  return $ L8.unpack $ responseBody response
 
urlsToContent :: [String] -> [IO String]
urlsToContent urls = do 
  map pullFromWeb urls

regex = "[N][^P](S|T)[^P]"

-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html
main = do
--    manager <- newManager tlsManagerSettings

--    content <- urlsToContent urlsActual
    --payload <- pullFromWeb "A2Z669"
    payload <- pullFromWeb fullEx
    
    let n = parseFasta payload
    
    let nameActual = (splitOn "|" $ fastaHeader $ n !! 0) !! 1

    let q = getAllMatches ((fastaSeq $ n !! 0) =~ regex)  :: [(Int, Int)]

    let qq = foldr (\(a, b) acc -> show a ++ " " ++ acc) "" q

--    putStrLn $ fastaHeader $ n !! 0
    -- huh, their indexes are 1-based

    putStrLn nameActual
    putStrLn qq



    let rando = urlsToContent urlsActual

    

    --x <- parseFasta rando

    -- can we print rando?
    
    

    putStr "foo"



--    payloads <- map (\s -> 
--                            do 
--                              request <- parseRequest s
--                              response <- httpLbs request manager  
--                              L8.unpack $ responseBody response) urlsActual
    

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





























