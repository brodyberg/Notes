-- {-# LANGUAGE TypeApplications #-}
-- :set -XTypeApplications

module Main where

import Data.Char (isAlphaNum, isSpace)  

newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving (Show, Eq)
newtype Error = Error String deriving (Show, Eq)

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
    lengthOk <- checkLength cleanedAlpha
    return (Password lengthOk)

validatePassword' :: Password -> Either Error Password
validatePassword' (Password password) = 
  cleanWhitespace password 
  >>= requireAlphaNum 
  >>= (\x -> Password <$> checkLength x)
    
-- USERNAME

checkLength :: String -> Either Error String
checkLength s = 
  case (length s > 20 || length s < 4) of 
    True -> Left (Error "Too long or short")
    _ -> Right s

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username
  >>= requireAlphaNum 
  >>= (\x -> Username <$> checkLength x)
  
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
      eq 1 (cleanWhitespace "    foo") (Right "foo")
      eq 2 (cleanWhitespace "foo") (Right "foo")
      eq 3 (cleanWhitespace "foo  ") (Right "foo  ")
      eq 4 (checkLength "") (Left (Error "Too long or short"))
      eq 5 (checkLength "julielovesbooks") (Right "julielovesbooks")
      eq 6 (checkLength "afasdfasdfasdfasdfasdfasdfasdf") (Left (Error "Too long or short"))
      eq 7 (validatePassword (Password "")) (Left (Error "Empty string"))
      eq 8 (validateUsername (Username "")) (Left (Error "Empty string"))
      eq 9 (validatePassword (Password "tee")) (Left (Error "Too long or short"))
      eq 10 (validateUsername (Username "tee")) (Left (Error "Too long or short"))
      eq 11 (validatePassword (Password "   tee")) (Left (Error "Too long or short"))
      eq 12 (validateUsername (Username "   tee")) (Left (Error "Too long or short"))
      eq 13 (validatePassword (Password "teeje")) (Right (Password "teeje"))
      eq 14 (validateUsername (Username "teeje")) (Right (Username "teeje"))
      eq 15 (validatePassword (Password "teejeerewereerereere3333rteter")) (Left (Error "Too long or short"))
      eq 16 (validateUsername (Username "teejeerewereerereere3333rteter")) (Left (Error "Too long or short"))
   
main :: IO ()
main = 
  -- putStr "Please enter a username\n> " >> getLine >>= (\l -> print $ validateUsername (Username l)) 
  -- >> putStr "Please enter a password\n> " >> getLine >>= (\l -> print $ validatePassword (Password l))

  putStr "Please enter a username\n> " >> getLine >>= print . validateUsername . Username >>
  putStr "Please enter a password\n> " >> getLine >>= print . validatePassword . Password


  -- putStr "Please enter a username\n> " >> 
  -- getLine >>= (print $ validateUsername . Username) 

-- main :: IO ()
-- main = 
--   do
--     putStr "Please enter a username\n> "
--     username <- Username <$> getLine
--     print (validateUsername username)

--     putStr "Please enter a password\n> "
--     password <- Password <$> getLine
--     print (validatePassword password)
