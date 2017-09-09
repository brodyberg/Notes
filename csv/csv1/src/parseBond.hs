module YouCanDoThis where

import Text.ParserCombinators.Parsec

-- bondFile :: GenParser Char st Char
-- bondFile = endBy namespace eol

-- I want a thing that just handles "namespace"
justNamespace :: GenParser Char st String
justNamespace = 
    do 
       namespace <- string "namespace"
       return namespace

-- namespace :: GenParser Char st Char
-- namespace = 
--     do string "namespace"
--        char ' '
--        return many (noneOf ",\n\r\"")

-- eol :: GenParser Char st Char
-- eol = 
--     do char '\n'
--        char '\r' <|> return '\n'
       

-- parseBondFile :: String -> Either ParseError [[String]]
-- parseBondFile = parse bondFile "(unknown)"
