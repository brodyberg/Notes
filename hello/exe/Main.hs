module Main where

import DogsRule
import Hello
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please type your name: "
  name <- getLine
  sayHello name
  dogs