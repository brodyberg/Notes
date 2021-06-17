module ProjectRosalind.Window ( windows ) where
  
--https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell

import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

-- https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell

-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:sequenceA

--ZipList 1

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

example = "NXSNXSN"