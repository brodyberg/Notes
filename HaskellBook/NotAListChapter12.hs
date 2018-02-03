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

-- use the initial on left
-- use cap on right
-- incoming is max
-- right is current
-- ensure current is < max

-- fundamentally I am building the tree
-- from the top down. From the top down I 
-- can label nodes from a high to a low
-- number as I drive the recursion toward
-- the base case 

-- but what they appear to be doing is 
-- building from the bottom up(?) If you 
-- build from the bottom up you can count
-- down toward zero from the leaves to the
-- root. But how to make that happen?

-- I definitely only know how to build 
-- trees from the root. So at the point
-- where there are n leaves due to the 
-- count, how do we make that and then
-- wrap that layer in a series of nodes
-- that hold them ultimately leading
-- to returning a single node for the
-- caller?

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

maker' :: Int
       -> Maybe(Int, Int, Int)
maker' 0 = Nothing
maker' x = Just(x - 1, x - 1, x - 1) 
       
unfold' :: (a -> Maybe (a, b, a))
        -> a
        -> BinaryTree b
unfold' f n = 
  case f n of 
    Just(l, c, r) -> if c == 2 then (Node Leaf 9 Leaf) else (Node Leaf 3 Leaf)
    _             -> Leaf

-- well, there's nothing to compare c to without
-- assuming the type of c so yeah, it (might) be
-- an instance of Eq, Ord and Show but we don't 
-- have another instance with which we can compare. 
-- what if we had a go function and were keeping
-- things around for the purpose?

treeBuild' :: Int -> BinaryTree Int
treeBuild' = unfold' maker'
    
  -- this fails because of course we don't know
  -- anything about a
  -- what we need to do is call maker and get back
  -- the Eq, Ord and Show-constrained b
  -- and then do something, like == check. 
  -- if n == 0 
  -- then Leaf
  -- else Node Leaf 9 Leaf

--  case f n of
-- whatever unfold' gets has to be compatible
-- with maker, but we can't know which maker we'll 
-- be using and can run no operation on n such 
-- that we constrain the type
-- but wait, the instance of BinaryTree includes
-- Eq, Ord and Show. So that requires a also 
-- is an instance of Eq, Ord and Show. 
-- we can definitely use Eq or Ord
-- we can instantly test this by running any function
-- of Eq or Ord on n










  -- Just(x, y, z) -> (Node (unfold f x) y (unfold f z))
  --   Nothing       -> Leaf
-- unfold' can't make judgements about n
-- so we build the tree based on 
-- Nothing or Just that's it, if we get Just, build
-- the tree, otherwise Leaf


-- 2. Make a tree builder. 
--    Using the unfold function you've made for 
--    BinaryTree, write the following function: 

treeBuild :: Int -> BinaryTree Int
treeBuild = unfold maker

  -- build while n < y
  -- we can't even pass that to ourselves!
  -- since we're recursive
  -- question: how to count node value up 
  -- from starting value

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