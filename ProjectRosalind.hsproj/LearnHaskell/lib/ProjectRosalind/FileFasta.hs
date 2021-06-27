module ProjectRosalind.FileFasta where

import System.IO (openFile, hGetContents, IOMode(ReadMode))
import Control.Monad

import ProjectRosalind.Fasta_Types
import ProjectRosalind.Fasta (parseFasta')
   
readLocalFile :: String -> IO String
readLocalFile path = do  
        handle <- openFile path ReadMode
        contents <- hGetContents handle
        return contents

filePathToFastas :: String -> IO [FastaSequence]
filePathToFastas path = do
  contents <- readLocalFile path
  return $ parseFasta' contents
