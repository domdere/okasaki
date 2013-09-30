module Data.BinaryTree 
    (   BinaryTree(..)
    ,   insert
    ,   member
    ,   fromList
    ) where

-- | A Binary Search Tree, from Chapter 2.2 of "Purely Functional Data Structures"

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

-- | Returns True if the object is in the tree:
--
-- prop> (\x -> not (x `member` Leaf)) (x :: Int)
--
member :: (Ord a) => a -> BinaryTree a -> Bool
member _ Leaf   = False
member x (Node y left right)
    | x == y    = True
    | x < y     = member x left
    | otherwise = member x right

-- | inserts an element into the tree..
--
insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
    | x < y = Node y (x `insert` left) right
    | otherwise = Node y left (x `insert` right)

-- | fromList converts a list of orderable elements into a BinaryTree containing those elements
--
-- >>> fromList [3,7,10,8,4,2,1]
-- Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 7 (Node 4 Leaf Leaf) (Node 10 (Node 8 Leaf Leaf) Leaf))
--
-- >>> (fromList []) :: BinaryTree Int
-- Leaf
--
-- >>> fromList [1]
-- Node 1 Leaf Leaf
--
fromList :: (Ord a) => [a] -> BinaryTree a
fromList = foldl (flip insert) Leaf

-- | gets the largest element in the tree..
--
-- prop> (\x xs -> getMax (fromList (x:xs)) == maximum (x:xs)) (x :: Int) (xs :: [Int])
--
getMax :: BinaryTree a -> Maybe a
getMax Leaf             = Nothing
getMax (Node x _ Leaf)  = Just x
getMax (Node _ _ right) = getMax right
