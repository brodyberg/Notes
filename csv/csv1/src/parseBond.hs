module YouCanDoThis where

import Text.ParserCombinators.Parsec

-- bondFile :: GenParser Char st String
bondFile = endBy namespaceDeclaration eol

namespaceDeclaration = 
    do 
       string "namespace"
       char ' '
       c <- many anyChar
       return c

namespace = 
    do 
       name <- sepBy block (char '.')    
       return name

block :: GenParser Char st String
block = 
    do 
        b <- many letter
        return b

eol :: GenParser Char st Char
eol = 
    do char '\n'
       char '\r' <|> return '\n'