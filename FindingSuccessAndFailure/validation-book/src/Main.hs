-- {-# LANGUAGE TypeApplications #-}
-- :set -XTypeApplications

module Main where

import Data.Char (isAlphaNum, isSpace)  

newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving (Show, Eq)
newtype Error = Error String deriving (Show, Eq)

checkPasswordLength :: Password -> Either Error Password
checkPasswordLength (Password password) = 
  case (length password > 20 || length password < 4) of 
    True -> Left (Error "Too long or short")
    _ -> Right (Password password)

requireAlphaNum :: Password -> Either Error Password
requireAlphaNum (Password xs) = 
  case (all isAlphaNum xs) of 
    True -> Right (Password xs)
    _ -> Left (Error "Not all characters are alpha-numeric")

cleanWhitespace :: Password -> Either Error Password
cleanWhitespace (Password "") = Left (Error "Empty string")
cleanWhitespace (Password (x:xs)) =
  case (isSpace x) of 
    True -> cleanWhitespace (Password xs)
    False -> Right (Password (x:xs))

validatePassword :: String -> Either Error Password
validatePassword password = 
  do
    cleaned <- cleanWhitespace (Password password)
    cleanedAlpha <- requireAlphaNum cleaned
    checkPasswordLength cleanedAlpha

validatePassword' :: String -> Either Error Password
validatePassword' password = cleanWhitespace (Password password) >>= requireAlphaNum >>= checkPasswordLength 
    
validatePassword'order :: String -> Either Error Password
validatePassword'order password = cleanWhitespace (Password password) >>= checkPasswordLength >>= requireAlphaNum

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
      eq 1 (checkPasswordLength (Password "")) (Left (Error "Too long or short"))
      eq 2 (checkPasswordLength (Password "julielovesbooks")) (Right (Password "julielovesbooks"))
      eq 3 (checkPasswordLength (Password "afasdfasdfasdfasdfasdfasdfasdf")) (Left (Error "Too long or short"))
      eq 4 (cleanWhitespace (Password "    foo")) (Right (Password "foo"))
      eq 5 (cleanWhitespace (Password "foo")) (Right (Password "foo"))
      eq 6 (cleanWhitespace (Password "foo  ")) (Right (Password "foo  "))
      
main :: IO ()
main = 
  do
    putStr "Please enter a password\n> "
    password <- getLine
    print (requireAlphaNum (Password password))
