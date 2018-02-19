module Main where

import Control.Monad (forever)
import Data.Char     (toLower)
import Data.Maybe    (isJust)
import Data.List     (intersperse)
import System.Exit   (exitSuccess)
import System.Random (randomRIO)

data Puzzle = 
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ 
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "../data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do 
  aw <- allWords
  return (filter gameLength aw)
  where 
    gameLength :: String -> Bool
    gameLength w = 
      let l = length w 
      in
        l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex 

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

zipper :: Char 
       -> Char 
       -> Maybe Char 
       -> Maybe Char
zipper guessed targetWordChar guessChar = 
  if guessed == targetWordChar
  then Just targetWordChar
  else guessChar

-- i thought zip stopped when one list ended, so 
-- how can this work if we've never guessed?
-- *Main> zipWith (\x y -> x) [1, 2, 3, 5] "ab"
-- [1,2]
fillInPuzzleChar :: Puzzle -> Char -> Puzzle
fillInPuzzleChar (Puzzle targetWord filledInSoFar s) c = 
  Puzzle targetWord newFilledInSoFar (c : s)
  where 
    newFilledInSoFar = 
      zipWith (zipper c) -- note we curry "down" to (a -> b -> b) for zipWith
        -- otherwise zipper would have too many parameters for zipWith to 
        -- accept
        targetWord filledInSoFar
-- so how can this ever start when zipWith is limited to 
-- going as long as the shortest list argument and filledInSoFar 
-- starts empty?
-- because we weren't properly setting up the Empty Puzzle with 
-- a list of Nothings per each character of the targetWord

-- String: word we're trying to guess
-- Characters we've filled in so far
-- Letters we've guessed so far

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess,
        alreadyGuessed puzzle guess) of 
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick something else!"
      return puzzle
    (True, _) -> do 
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return (fillInPuzzleChar puzzle guess)
    (False, _) -> do 
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInPuzzleChar puzzle guess)

gameOver:: Puzzle -> IO ()
gameOver (Puzzle targetWord _ guessed) = 
  if (length guessed) > (length targetWord) 
  then do putStrLn "You lose!"
          putStrLn $ "The word was: " ++ targetWord
          exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do 
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be\
                    \ a single letter."

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (take (length s) $ repeat Nothing) []

main :: IO ()
main = do
  word <- randomWord' 
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle