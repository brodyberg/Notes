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

mapOk :: IO ()
mapOk = 
  if mapTree (+1) testTree' == mapExpected
  then putStrLn "map ok"
  else putStrLn "map not ok"

-- Write functions that convert a BinaryTree values
-- to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = 
  a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = 
  (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = 
  (postorder left) ++ (postorder right) ++ [a]

--foldr' :: (a -> b) -> b -> b
foldr' :: (a -> b -> b) -> BinaryTree a -> b -> b
foldr' _ Leaf acc = acc 
foldr' f (Node left a right) acc = 
  --f a acc 
  (\a acc -> f a + acc) -- + is troubling me
  -- how do we know a and b support (+)?
  -- and then what, we have another + to 
  -- combine with the rest of the tree?
  -- or do we map then reduce? seems easier
  -- map f over whole tree
  -- then do inorder but still we have to 
  -- know how to combine a and b types

testTree :: BinaryTree Integer
testTree = 
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)
  
testPreorder :: IO ()
testPreorder = 
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder: ok"
  else putStrLn "Preorder: fail"

testInorder :: IO ()
testInorder = 
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder: ok"
  else putStrLn "Inorder: fail"

testPostorder :: IO ()
testPostorder = 
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder: ok"
  else putStrLn "Postorder: fail"

testFoldr :: IO ()
testFoldr = 
  if foldr' (+5) testTree 0 == 23
  then putStrLn "Foldr': ok"
  else putStrLn "Foldr': fail"

main :: IO ()
main = do 
  mapOk
  testPreorder
  testInorder
  testPostorder
  testFoldr

