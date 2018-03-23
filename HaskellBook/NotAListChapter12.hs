module NotAList where

-- Given the BinaryTree from Chapter 11, complete
-- the following exercises. 

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree

unfold :: (a -> Maybe(a, b, a))
       -> a
       -> BinaryTree b
unfold f x = 
  case f x of
    Just(l, c, r) -> Node (unfold f l) c (unfold f r)
    _             -> Leaf

treeBuild :: Int
          -> BinaryTree Int
treeBuild x = 
  unfold (\n -> if n < x then Just(n + 1, n, n + 1) else Nothing) 0

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