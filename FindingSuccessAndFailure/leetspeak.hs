substituteChar :: Char -> Char
substituteChar c =
  case c of 
    'e' -> '3'
    'o' -> '0'
    'a' -> '4'
    't' -> '7'
    _ -> c

translateWord word = map substituteChar word

main :: IO ()
main = 
  do 
    putStr "Please enter a word.\n"
    word1 <- getLine
    print (translateWord word1)