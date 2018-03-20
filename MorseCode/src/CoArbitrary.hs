{-# LANGUAGE DeriveGeneric #-}

module CoArbitrary where

import GHC.Generics
import Test.QuickCheck

data Bool' = 
    True' 
  | False' 
  deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary
 
falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

