module Main where

import Lib (someFunc, name, purplePearl)
import Control.Lens (view)

main :: IO ()
main = someFunc $ view name purplePearl
