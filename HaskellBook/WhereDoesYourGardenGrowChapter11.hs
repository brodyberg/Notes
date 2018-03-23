module WhereDoesYourGardenGrow where

-- 1. Given the type

-- data FlowerType =
--     Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving Show

type Gardener = String

-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

-- What is the sum of products form of Garden?

data Garden = 
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show