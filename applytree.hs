
data BinaryTree a =
                Leaf
              | Node (BinaryTree a) a (BinaryTree a)
              deriving (Eq, Ord, Show)
            


-- filling in some details to help you along
-- Note, you do *not* need to use insert' for this.
-- Retain the original structure of the tree.
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

    -- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"
    
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right


postorder :: BinaryTree a -> [a]
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
  
  
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b 
foldTree f acc root = foldr f acc (inorder root)


myMap :: ( a-> b) -> [a] -> [b]
myMap f xs = 
  foldr (\x y -> f x : y) [] xs

mapTree' :: (a -> b)
         -> BinaryTree a
         -> BinaryTree b
mapTree' f bt =
  foldTree (\a b -> Node Leaf (f a) b) Leaf bt

  