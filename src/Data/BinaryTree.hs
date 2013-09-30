module Data.BinaryTree 
    (   BinaryTree(..)
    ,   insert
    ,   member
    ,   fromList
    ,   removedFrom
    ) where

-- | A Binary Search Tree, from Chapter 2.2 of "Purely Functional Data Structures"
-- Work is done in Chapters 3 and 4 to look into how to keep Trees balanced
--
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

-- | Removes a given element if it exists in the tree,
-- otherwise it returns the same tree.
--
-- >>> 2 `removedFrom` fromList [2,1,3]
-- Node 1 Leaf (Node 3 Leaf Leaf)
--
-- >>> 1 `removedFrom` fromList [2,1,3]
-- Node 2 Leaf (Node 3 Leaf Leaf)
--
-- >>> 3 `removedFrom` fromList [2,1,3]
-- Node 2 (Node 1 Leaf Leaf) Leaf
--
removedFrom :: (Ord a) => a -> BinaryTree a -> BinaryTree a
removedFrom _ Leaf = Leaf
removedFrom x (Node x' left right)
    | x == x'   = left `mergeTrees` right
    | x < x'    = Node x' (x `removedFrom` left) right
    | otherwise = Node x' left (x `removedFrom` right)

-- | merges two trees where all the elements in right are greater than
-- all the elements in the left.
--
-- >>> (fromList [2,1,3]) `mergeTrees` (fromList [5,4,6]) :: BinaryTree Int
-- Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 5 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf)))
--
mergeTrees :: BinaryTree a -> BinaryTree a -> BinaryTree a
mergeTrees Leaf t = t
mergeTrees t Leaf = t
mergeTrees (Node x left' right') right = Node x left' (right' `mergeTrees` right)
