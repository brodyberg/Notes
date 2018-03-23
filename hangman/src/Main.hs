module Main where

import Control.Monad (forever)
import Data.Char     (toLower)
import Data.Maybe    (isJust)
import Data.List     (intersperse)
import System.Exit   (exitSuccess)
import System.Random (randomRIO)

data Puzzle = 
  Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle s discovered guessed guessCount) = 
    (intersperse ' ' $ 
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    ++ " Guessed Remaining: " ++ (length s) - guessCount

newtype WordList = 
  WordList [String]
  deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "../data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do 
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where 
    gameLength :: String -> Bool
    gameLength w = 
      let l = length w 
      in
        l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex 

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = elem c g

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

fillInPuzzleChar :: Puzzle -> Char -> Puzzle
fillInPuzzleChar (Puzzle targetWord filledInSoFar s, _) c = 
  Puzzle targetWord newFilledInSoFar (c : s)
  where 
    newFilledInSoFar = 
      zipWith (zipper c) -- note we curry "down" to (a -> b -> b) for zipWith
        -- otherwise zipper would have too many parameters for zipWith to 
        -- accept
        targetWord filledInSoFar

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
      return loseTurn (fillInPuzzleChar puzzle guess)

loseTurn :: Puzzle -> Puzzle
loseTurn (Puzzle s f g t) = (Puzzle s f g (t + 1))

gameOver:: Puzzle -> IO ()
gameOver (Puzzle targetWord _ guessed guessCount) = 
  if guessCount > (length targetWord) 
  then do putStrLn "You lose!"
          putStrLn $ "The word was: " ++ targetWord
          exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = 
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
freshPuzzle s = Puzzle s (take (length s) $ repeat Nothing) [] 0

main :: IO ()
main = do
  word <- randomWord' 
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle