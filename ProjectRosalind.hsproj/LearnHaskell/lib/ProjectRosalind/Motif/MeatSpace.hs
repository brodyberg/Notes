module ProjectRosalind.Motif.MeatSpace where
  
import ProjectRosalind.Motif.Combinator

import Text.Parsec.String (Parser)
import ProjectRosalind.Motif.Char (anyChar)
import ProjectRosalind.Motif.Char
import ProjectRosalind.Motif.FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
import ProjectRosalind.Motif.Combinator (many1)

x = regularParse anyChar "a"

-- this file demonstrates that we can bring in all the files required in order to read this tutorial on Parsec: 

-- https://jakewheat.github.io/intro_to_parsing/#_overview