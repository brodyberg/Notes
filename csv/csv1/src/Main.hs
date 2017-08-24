module Main where

import Csv1

main :: IO ()
main = do
  case (parseCSV "hi\n") of 
    Left msg -> putStrLn msg
    Right output -> putStrLn output

  putStrLn "hello world"

-- putList :: [[String]] -> IO ()
-- putList [] = ()
-- putList (x:xs) = do
--   putStr x