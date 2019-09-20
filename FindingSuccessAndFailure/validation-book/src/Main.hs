-- {-# LANGUAGE TypeApplications #-}
-- :set -XTypeApplications

module Main where

import Data.Char (isAlphaNum, isSpace)  

newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving (Show, Eq)
newtype Error = Error String deriving (Show, Eq)

checkPasswordLength :: String -> Either Error Password
checkPasswordLength s = 
  case (length s > 20 || length s < 4) of 
    True -> Left (Error "Too long or short")
    _ -> Right (Password s)

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs = 
  case (all isAlphaNum xs) of 
    True -> Right xs
    _ -> Left (Error "Not all characters are alpha-numeric")

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Empty string")
cleanWhitespace (x:xs) =
  case (isSpace x) of 
    True -> cleanWhitespace xs
    False -> Right (x:xs)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) = 
  do
    cleaned <- cleanWhitespace password
    cleanedAlpha <- requireAlphaNum cleaned
    checkPasswordLength cleanedAlpha

validatePassword' :: Password -> Either Error Password
validatePassword' (Password password) = 
  cleanWhitespace password 
  >>= requireAlphaNum 
  >>= checkPasswordLength 
    
-- TESTS

printTestResult :: Either Error () -> IO ()
printTestResult r = 
  case r of
    Left (Error err) -> putStrLn err
    Right () -> putStrLn "All Tests Pass"

eq :: (Eq a, Show a) => Int -> a -> a -> Either Error ()
eq n actual expected = 
  case (actual == expected) of 
    True -> Right ()
    False -> Left (Error (unlines 
      [ "Test " ++ show n 
      , "  Expected:  " ++ show expected
      , "  But got:   " ++ show actual
      ]))

test :: IO ()
test = printTestResult $ 
    do
      eq 1 (checkPasswordLength "") (Left (Error "Too long or short"))
      eq 2 (checkPasswordLength "julielovesbooks") (Right (Password "julielovesbooks"))
      eq 3 (checkPasswordLength "afasdfasdfasdfasdfasdfasdfasdf") (Left (Error "Too long or short"))
      eq 4 (cleanWhitespace "    foo") (Right "foo")
      eq 5 (cleanWhitespace "foo") (Right "foo")
      eq 6 (cleanWhitespace "foo  ") (Right "foo  ")
      
main :: IO ()
main = 
  do
    putStr "Please enter a password\n> "
    password <- getLine
    print (validatePassword (Password password))
