module Csv1 where

-- file: ch16/csv1.hs

import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = 
    do 
        result <- many line
        eof
        return result

csvFile' :: GenParser Char st [[String]]
csvFile' = 
    do 
        result <- many line2
        eof
        return result

line2 :: GenParser Char st [String]
line2 = 
    do 
        result <- many cellContent
        eol 
        return result

line :: GenParser Char st [String]
line = 
    do 
        result <- cells
        eol
        return result
    
p1 :: GenParser Char () [String] -> String -> Either ParseError [String]
p1 x input = parse x "(unknown)" input 

p2 :: GenParser Char () [[String]] -> String -> Either ParseError [[String]]
p2 x input = parse x "(unknown)" input 

-- cell :: GenParser Char st [String]
-- cell = 
--     do


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