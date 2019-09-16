module BinarySearch where

length :: Int
length = 4

data Color = 
    Lightest
  | Light
  | Medium
  | Dark
  deriving (Eq, Show)

data Visited = 
    Yes
  | No
  deriving (Eq, Show)

data Exit = 
    N
  | E
  deriving (Eq, Show)

data Cell = Cell Exit Visited Color
  deriving (Eq, Show)

-- let c = Cell N No Medium

-- data RowType = 
--     First
--   | Last
--   deriving (Eq, Show)

-- data Row :: [Cell] RowType 
--   deriving (Eq, Show)

-- data Maze :: [Row]
--   deriving (Eq, Show)

-- maze :: [[Exit]]
-- maze = [[N, E, E, N]]

-- walkRow :: [Exit] -> String
-- walkRow []     = "|"
-- walkRow (x:xs) = 

-- -- for this whole thing in show we could 
-- -- write out html/svg

-- -- main :: IO ()
-- -- main = do
-- --   putStr " _"
-- --   putStr "| |"
-- --   putStr " -"
