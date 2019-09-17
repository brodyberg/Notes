--{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (isAlphaNum, isSpace)

checkPasswordLength :: String -> Maybe String
checkPasswordLength password = 
  case (length password > 20 || length password < 4) of
    True -> Nothing
    _ -> Just password

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs = 
  case (all isAlphaNum xs) of 
    True -> Just xs
    _ -> Nothing

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x:xs) =
  case (isSpace x) of 
    True -> cleanWhitespace xs
    False -> Just (x:xs)

checkAllThree :: String -> Maybe String
checkAllThree password = 
  case cleanWhitespace password of 
    Just cleaned -> 
      case requireAlphaNum cleaned of
        Just cleanedAlpha -> 
          case checkPasswordLength cleanedAlpha of
            Just cleanAlphaSized -> Just cleanAlphaSized
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

checkAllThreeAgain :: String -> Maybe String
checkAllThreeAgain password = 
  do 
    cleaned <- cleanWhitespace password
    cleanedAlpha <- requireAlphaNum cleaned
    checkPasswordLength cleanedAlpha

checkAllThreeAgain' :: String -> Maybe String
checkAllThreeAgain' password = 
  cleanWhitespace password >>= requireAlphaNum >>= checkPasswordLength
  
-- *Main> :type (>>=) @IO @String

-- exercise: convert this to do syntax: 
reverseLine :: IO ()
-- reverseLine = getLine >>= (print . reverse)
reverseLine = 
  do 
    line <- getLine
    (print . reverse) line

main :: IO ()
main = 
  do
    putStr "Please enter a password\n> "
    password <- getLine
    print (requireAlphaNum password)
    -- print (checkPasswordLength password && requireAlphaNum password)
