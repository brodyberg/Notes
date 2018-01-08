module BinaryTree where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

insert' :: Ord a 
            => a
            -> BinaryTree a
            -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
  -- note: idempotent insert
  | b == a = Node left a right 
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- Write map for BinaryTree

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
--  Node undefined undefined undefined
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)
      
mapExpected = 
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOk = 
  if mapTree (+1) testTree' == mapExpected
  then print "map ok"
  else error "map not ok"
