module ProjectRosalind.FindingAProteinMotif where
  
-- make a dfa that recognizes a DSL for motifs
-- we could easily take the Fasta lib and learn from 
-- it to author a parser for this and then run it
-- on the output of http requests given the 
-- UniProt Protein Database access IDs.

-- https://www.uniprot.org/uniprot/B5ZC00.fasta
-- https://www.uniprot.org/uniprot/A2Z669.fasta
-- https://www.uniprot.org/uniprot/P07204.fasta
-- 
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- we get list of ids
-- make list of urls
-- retrieve list of docs
-- read each doc for match string indexes
-- print name and indexes

main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "https://www.uniprot.org/uniprot/P07204.fasta"
    response <- httpLbs request manager  
    L8.putStrLn $ responseBody response
