module Main where

import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password = 
  case (length password < 20) of
    True -> Just password
    _ -> Nothing

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs = 
  case (all isAlphaNum xs) of 
    True -> Just xs
    _ -> Nothing

main :: IO ()
main = 
  do
    putStr "Please enter a password\n> "
    password <- getLine
    print (requireAlphaNum password)
    -- print (checkPasswordLength password && requireAlphaNum password)
