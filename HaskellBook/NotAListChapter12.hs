module NotAList where

-- Given the BinaryTree from Chapter 11, complete
-- the following exercises. 

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree

-- the secret here is that *I* define what is sent
-- into unfold, so I can specify a and b to what I want
-- for the BinaryTree

maker :: Int
      -> Maybe(Int, Int, Int)
maker 0 = Nothing
maker x = Just(x - 1, x - 1, x - 1) 

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f n = 
  case f n of
    Just(x, y, z) -> (Node (unfold f x) y (unfold f z))
    Nothing       -> Leaf

-- 2. Make a tree builder. 
--    Using the unfold function you've made for 
--    BinaryTree, write the following function: 

treeBuild :: Int -> BinaryTree Int
treeBuild = unfold maker 

-- treeBuild 0
-- Leaf

-- treeBuild 1
-- Node Leaf 0 Leaf

-- treeBuild 2
-- Node (Node Leaf 1 Leaf)
--      0
--      (Node Leaf 1 Leaf)

-- treeBuild 3
-- Node (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf)
--      0 
--      (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf)