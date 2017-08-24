module Csv1 where

-- file: ch16/csv1.hs

import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = 
    do 
        result <- many line
        eof
        return result

line :: GenParser Char st [String]
line = 
    do 
        result <- cells
        eol
        return result
    
cells :: GenParser Char st [String]
cells = 
    do 
        first <- cellContent
        next <- remainingCells
        return (first : next)
    
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)
    <|> (return [])

cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input