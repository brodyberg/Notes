module UsingWriter where

import Control.Monad.Writer

op1 :: Int -> Writer [String] Int
op1 x = writer (x, ["Working on: " ++ show x])

op2 :: Int -> Writer [String] Int
op2 x = writer (x, ["Computing: " ++ show x])

operations :: Writer [String] Int
operations = do
    tell ["Starting"]
    left <- op1 4
    right <- op2 6
    return (left * right)
