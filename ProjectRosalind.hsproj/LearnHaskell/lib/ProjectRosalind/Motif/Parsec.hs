-- https://raw.githubusercontent.com/JakeWheat/intro_to_parsing/master/Text/Parsec/String/Parsec.hs

module ProjectRosalind.Motif.Parsec where

{-

wrapper for some Text.Parsec functions which use a simplified type

-}

import Text.Parsec.String (Parser)
import qualified Text.Parsec as P


try :: Parser a -> Parser a
try = P.try

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse