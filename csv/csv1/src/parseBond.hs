module YouCanDoThis where

import Text.ParserCombinators.Parsec

-- bondFile :: GenParser Char st String
bondFile = endBy namespaceDeclaration eol

-- namespace :: GenParser Char st String
namespaceDeclaration = 
    do 
       string "namespace"
       char ' '
--        return "foo"
       c <- many anyChar
       return c
       --        return many (noneOf "\n")

namespace = 
    do 
--       name <- sepBy string (char '.')    
       name <- sepBy block (char '.')    
       return name

-- block :: GenParser Char st Char
block :: GenParser Char st String
block = 
    do 
        b <- many letter
        return b

-- -- I want a thing that just handles "namespace"
-- justNamespace :: GenParser Char st String
-- justNamespace = 
--     do 
--        namespace <- string "namespace"
--        return namespace

-- namespace :: GenParser Char st Char
-- namespace = 
--     do string "namespace"
--        char ' '
--        return many (noneOf ",\n\r\"")

eol :: GenParser Char st Char
eol = 
    do char '\n'
       char '\r' <|> return '\n'
       

-- parseBondFile :: String -> Either ParseError [[String]]
-- parseBondFile = parse bondFile "(unknown)"
