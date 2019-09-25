-- {-# LANGUAGE TypeApplications #-}
-- :set -XTypeApplications

module Main where

import Data.Char (isAlphaNum, isSpace)  
import Data.Validation

newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving (Show, Eq)
newtype Error = Error [String] deriving (Show, Eq)

data User = User Username Password deriving Show

--------------

makeUser :: Username -> Password -> Either Error User
makeUser = undefined
-- makeUser username password = 

--------------

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs = 
  case (all isAlphaNum xs) of 
    True -> Success xs
    _ -> Failure (Error ["Not all characters are alpha-numeric"])

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Empty string"])
cleanWhitespace (x:xs) =
  case (isSpace x) of 
    True -> cleanWhitespace xs
    False -> Success (x:xs)

-- validatePassword :: Password -> Validation Error Password
-- validatePassword (Password password) = 
--   do
--     cleaned <- cleanWhitespace password
--     cleanedAlpha <- requireAlphaNum cleaned
--     lengthOk <- checkLength cleanedAlpha
--     return (Password lengthOk)

-- validatePassword' :: Password -> Validation Error Password
-- validatePassword' (Password password) = 
--   cleanWhitespace password 
--   >>= requireAlphaNum 
--   >>= (\x -> Password <$> checkLength x)
    
-- USERNAME

checkLength :: String -> Validation Error String
checkLength s = 
  case (length s > 20 || length s < 4) of 
    True -> Failure (Error ["Too long or short"])
    _ -> Success s

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  cleanWhitespace username
  >>= requireAlphaNum 
  >>= (\x -> Username <$> checkLength x)
  
-- TESTS

printTestResult :: Validation Error () -> IO ()
printTestResult r = 
  case r of
    Failure (Error err) -> putStrLn (concat err)
    Success () -> putStrLn "All Tests Pass"

eq :: (Eq a, Show a) => Int -> a -> a -> Validation Error ()
eq n actual expected = 
  case (actual == expected) of 
    True -> Success ()
    False -> Failure (Error [(unlines 
      [ "Test " ++ show n 
      , "  Expected:  " ++ show expected
      , "  But got:   " ++ show actual
      ])])

test :: IO ()
test = printTestResult $ 
    do
      eq 1 (cleanWhitespace "    foo") (Success "foo")
      eq 2 (cleanWhitespace "foo") (Success "foo")
      eq 3 (cleanWhitespace "foo  ") (Success "foo  ")
      eq 4 (checkLength "") (Failure (Error ["Too long or short"]))
      eq 5 (checkLength "julielovesbooks") (Success "julielovesbooks")
      eq 6 (checkLength "afasdfasdfasdfasdfasdfasdfasdf") (Failure (Error ["Too long or short"]))
      eq 7 (validatePassword (Password "")) (Failure (Error ["Empty string"]))
      eq 8 (validateUsername (Username "")) (Failure (Error ["Empty string"]))
      eq 9 (validatePassword (Password "tee")) (Failure (Error ["Too long or short"]))
      eq 10 (validateUsername (Username "tee")) (Failure (Error ["Too long or short"]))
      eq 11 (validatePassword (Password "   tee")) (Failure (Error ["Too long or short"]))
      eq 12 (validateUsername (Username "   tee")) (Failure (Error ["Too long or short"]))
      eq 13 (validatePassword (Password "teeje")) (Success (Password "teeje"))
      eq 14 (validateUsername (Username "teeje")) (Success (Username "teeje"))
      eq 15 (validatePassword (Password "teejeerewereerereere3333rteter")) (Failure (Error ["Too long or short"]))
      eq 16 (validateUsername (Username "teejeerewereerereere3333rteter")) (Failure (Error ["Too long or short"]))
   
main :: IO ()
main = 
  putStr "Please enter a username\n> " >> getLine >>= print . validateUsername . Username >>
  putStr "Please enter a password\n> " >> getLine >>= print . validatePassword . Password
