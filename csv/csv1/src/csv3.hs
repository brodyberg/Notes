-- From Real-World Haskell by Brian O'Sullivan etc. 
import Text.ParserCombinators.Parsec

-- eol :: Text.Parsec.Prim.ParsecT String () () String
-- eol :: GenParser Char st Char
eol :: GenParser Char st [Char]
eol = string "\n" <|> string "\n\r"

eol2 :: GenParser Char st [Char]
eol2 = string "\n\r" <|> string "\n"


-- how to call this thing?

-- parse eol "" "\n"

