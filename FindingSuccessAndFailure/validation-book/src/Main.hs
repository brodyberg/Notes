-- {-# LANGUAGE TypeApplications #-}
-- :set -XTypeApplications

module Main where

import Data.Char (isAlphaNum, isSpace)

-- EITHER

checkPasswordLength' :: String -> Either String String
checkPasswordLength' password = 
  case (length password > 20 || length password < 4) of 
    True -> Left "Too long or short"
    _ -> Right password

requireAlphaNum' :: String -> Either String String
requireAlphaNum' xs = 
  case (all isAlphaNum xs) of 
    True -> Right xs
    _ -> Left "Not all characters are alpha-numeric"

cleanWhitespace' :: String -> Either String String
cleanWhitespace' "" = Left "Empty string"
cleanWhitespace' (x:xs) =
  case (isSpace x) of 
    True -> cleanWhitespace' xs
    False -> Right (x:xs)

validatePassword' :: String -> Either String String
validatePassword' password = 
  do
    cleaned <- cleanWhitespace' password
    cleanedAlpha <- requireAlphaNum' cleaned
    checkPasswordLength' cleanedAlpha

validatePassword'' :: String -> Either String String
validatePassword'' password = cleanWhitespace' password >>= requireAlphaNum' >>= checkPasswordLength' 
    
-- MAYBE

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

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x 
-- *Main> bindMaybe Nothing (\x -> Just x)
-- Nothing
-- *Main> bindMaybe (Just 1) (\x -> Just (x + 2))
-- Just 3

data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue 
  :: StringOrValue a 
  -> (a -> StringOrValue b)
  -> StringOrValue b
bindStringOrValue (Str s) f = (Str s)
bindStringOrValue (Val x) f = f x

-- bindStringOrValue (Str "foo") (\x -> (Str "bar"))
-- bindStringOrValue (Str "foo") (\x -> (Val x))
-- bindStringOrValue (Val 1) (\x -> (Str "bar"))
-- bindStringOrValue (Val 1) (\x -> (Val (x + 1)))

main :: IO ()
main = 
  do
    putStr "Please enter a password\n> "
    password <- getLine
    print (requireAlphaNum password)
    -- print (checkPasswordLength password && requireAlphaNum password)
