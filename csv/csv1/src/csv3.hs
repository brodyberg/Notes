module YouCanDoThis where

-- From Real-World Haskell by Brian O'Sullivan etc. 
import Text.ParserCombinators.Parsec

-- eol :: Text.Parsec.Prim.ParsecT String () () String
-- eol :: GenParser Char st Char
eol :: GenParser Char st [Char]
eol = string "\n" <|> string "\n\r"

eol2 :: GenParser Char st [Char]
eol2 = string "\n\r" <|> string "\n"

eol3 :: GenParser Char st Char
eol3 = 
    do char '\n'
       char '\r' <|> return '\n'

-- says: require an 'a'
-- if you get a 'b' that's great, return 'b'
-- if no 'b' though, return 'a'
fooOption3 :: GenParser Char st Char
fooOption3 = 
    do char 'a'
       char 'b' <|> return 'a'
       
-- first attempt at creating a function 
-- taking a parser and a parameter and 
-- then using it. doesn't compile
-- I don't know how to work with contained
-- types like GenParser
-- myTest1 :: GenParser Char st Char -> Char
-- myTest1 c =
--     do char 'a'
--        char c <|> return 'a' 

-- how does do know we're working in the 
-- ParsecT monad?
-- How does one work with things like this
-- and any other value as a parameter?
-- myTest2 :: GenParser Char st Char -> Char
-- myTest2 c =
--     do char 'a'
--        char 'b' <|> return 'a' 

eol4 :: GenParser Char st Char
eol4 = 
    do option '\n' (char '\r')

-- option is way different from above
-- IFF we get an 'a' return 'a'
-- IFF we get ANY OPTHER CHARACTER OTHER THAN 'b', return 'a'
-- But if we get a 'b', return 'b'
fooOption4 :: GenParser Char st Char
fooOption4 =
    do option 'a' (char 'b')
    
eol5 :: GenParser Char st [Char]
eol5 =   try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\n"
     <|> string "\r"


