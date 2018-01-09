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

foldr' :: (a -> b -> b) 
       -> b 
       -> BinaryTree a 
       -> b
foldr' f acc tree = 
  foldr f acc $ inorder tree

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
  if foldr' (+) 0 testTree == 6
    then putStrLn "Foldr': ok"
    else putStrLn "Foldr': fail"

testFoldr2 :: IO ()
testFoldr2 = 
  if foldr' (\x acc -> x + 5 + acc) 0 testTree == 21
    then putStrLn "Foldr' 2: ok"
    else putStrLn "Foldr' 2: fail"
    
main :: IO ()
main = do 
  mapOk
  testPreorder
  testInorder
  testPostorder
  testFoldr
  testFoldr2

