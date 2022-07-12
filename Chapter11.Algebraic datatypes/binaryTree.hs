module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' ::
  Ord a =>
  a ->
  BinaryTree a ->
  BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- ----------------------------------------------------------------------------
-- Write map for BinaryTree
-- ----------------------------------------------------------------------------

mapTree ::
  (a -> b) ->
  BinaryTree a ->
  BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' ::
  BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "okay!"
    else error "test failed!"

-- ----------------------------------------------------------------------------
-- Convert binary trees to lists
-- ----------------------------------------------------------------------------

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) =
  [a] ++ preOrder left ++ preOrder right

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) =
  inOrder left ++ [a] ++ inOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) =
  postOrder left ++ postOrder right ++ [a]

testTreeForToList :: BinaryTree Integer
testTreeForToList =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreOrder :: IO ()
testPreOrder =
  if preOrder testTreeForToList == [2, 1, 3]
    then putStrLn "PreOrder fine!"
    else putStrLn "Bad news bears"

testInOrder :: IO ()
testInOrder =
  if inOrder testTreeForToList == [1, 2, 3]
    then putStrLn "InOrder fine!"
    else putStrLn "Bad news bears"

testPostOrder :: IO ()
testPostOrder =
  if postOrder testTreeForToList == [1, 3, 2]
    then putStrLn "PostOrder fine!"
    else putStrLn "Bad news bears"

-- ----------------------------------------------------------------------------
-- Write foldr for Binary Tree
-- ----------------------------------------------------------------------------

foldTree ::
  (a -> b -> b) ->
  b ->
  BinaryTree a ->
  b
foldTree _ b Leaf = b
foldTree f b (Node left a right) =
  -- lb: left value
  -- nb: node value
  let lb = foldTree f b left
      nb = f a lb
   in foldTree f nb right
