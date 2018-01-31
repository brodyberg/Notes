module NotAList where

-- Given the BinaryTree from Chapter 11, complete
-- the following exercises. 

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold _ 0 = Leaf
unfold f n = Node (unfold f (n - 1)) n (unfold f (n - 1))

-- 2. Maybe a tree builder. 
--    Using the unfold function you've made for 
--    BinaryTree, write the following function: 

treeBuild :: Integer -> BinaryTree Integer
treeBuild = undefined

-- treeBuild 0
-- Lead

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