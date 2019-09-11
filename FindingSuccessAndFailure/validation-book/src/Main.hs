module Main where

checkPasswordLength :: String -> Maybe String
checkPasswordLength password = 
  case (length password < 20) of
    True -> Just password
    _ -> Nothing

main :: IO ()
main = 
  do
    putStr "Please enter a password\n> "
    password <- getLine
    print (checkPasswordLength password)
