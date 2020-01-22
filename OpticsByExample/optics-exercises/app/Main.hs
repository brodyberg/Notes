module Main where

import Lib (someFunc, purplePearl, name)
import Control.Lens (view)

main :: IO ()
main = someFunc $ view name purplePearl
