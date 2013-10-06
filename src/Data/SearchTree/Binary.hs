module Data.SearchTree.Binary
    (   BinaryTree(..)
    ) where

import Data.SearchTree

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.List
-- >>> import Test.QuickCheck
--
-- >>> newtype NoDupes a = NoDupes (BinaryTree a) deriving (Show)
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (BinaryTree a) where arbitrary = fromList <$> arbitrary
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (NoDupes a) where arbitrary = (NoDupes . fromList . nub) <$> arbitrary

-- | A Binary Search Tree, from Chapter 2.2 of "Purely Functional Data Structures"
-- Work is done in Chapters 3 and 4 to look into how to keep Trees balanced
--
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

-- | Returns True if the object is in the tree:
--
-- prop> (\x -> not (x `member'` Leaf)) (x :: Int)
--
member' :: (Ord a) => a -> BinaryTree a -> Bool
member' _ Leaf   = False
member' x (Node y left right)
    | x == y    = True
    | x < y     = member' x left
    | otherwise = member' x right

-- | inserts an element into the tree..
--
insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node x Leaf Leaf
insert' x (Node y left right)
    | x < y = Node y (x `insert'` left) right
    | otherwise = Node y left (x `insert'` right)

-- | Implement insert for Binary Tree again such that dupes are not added.
--
-- prop> (\x t -> ((==) =<< (insertIfMissing' x)) (x `insertIfMissing'` t)) (x :: Int) (t :: BinaryTree Int)
--
-- >>> 2 `insertIfMissing'` Leaf
-- Node 2 Leaf Leaf
--
-- >>> 2 `insertIfMissing'` (2 `insertIfMissing'` Leaf)
-- Node 2 Leaf Leaf
--
insertIfMissing' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertIfMissing' x Leaf = Node x Leaf Leaf
insertIfMissing' x t@(Node x' left right)
    | x == x'   = t
    | x < x'    = Node x' (x `insertIfMissing'` left) right
    | otherwise = Node x' left (x `insertIfMissing'` right)

-- | Removes a given element if it exists in the tree,
-- otherwise it returns the same tree.
--
-- >>> 2 `removedFrom'` fromList [2,1,3]
-- Node 3 (Node 1 Leaf Leaf) Leaf
--
-- >>> 1 `removedFrom'` fromList [2,1,3]
-- Node 2 Leaf (Node 3 Leaf Leaf)
--
-- >>> 3 `removedFrom'` fromList [2,1,3]
-- Node 2 (Node 1 Leaf Leaf) Leaf
--
removedFrom' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
removedFrom' _ Leaf = Leaf
removedFrom' x (Node x' left right)
    | x == x'   = left `mergeTrees` right
    | x < x'    = Node x' (x `removedFrom'` left) right
    | otherwise = Node x' left (x `removedFrom'` right)

-- Instances

instance SearchTree BinaryTree where
    emptyTree = Leaf
    treeInsert = insert'
    treeInsertIfMissing = insertIfMissing'
    treeMember = member'
    removedFromTree = removedFrom'

-- Helpers

checkMember :: (Ord a) => BinaryTree a -> a -> Bool
checkMember t x = not (x `member'` t)

checkMaybeMember :: (Ord a) => (Maybe a, BinaryTree a) -> Bool
checkMaybeMember (x, t) = maybe True (checkMember t) x

-- | remove the max element from tree and return both the max element
-- if there is one and the tree with the element removed.
--
-- prop> (\(NoDupes t) -> checkMaybeMember (removeMaxElement t)) (t :: NoDupes Int)
--
-- >>> removeMaxElement (fromList [2,3,5,7,4,1])
-- (Just 7,Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 5 (Node 4 Leaf Leaf) Leaf)))
--
-- >>> removeMaxElement Leaf
-- (Nothing,Leaf)
--
removeMaxElement :: (Ord a) => BinaryTree a -> (Maybe a, BinaryTree a)
removeMaxElement Leaf = (Nothing, Leaf)
removeMaxElement (Node x Leaf Leaf) = (Just x, Leaf)
removeMaxElement (Node x left right) = (maxElement, Node x left newRight)
    where (maxElement, newRight) = removeMaxElement right

-- | remove the min element from tree and return both the min element
-- if there is one and the tree with the element removed.
--
-- prop> (\(NoDupes t) -> checkMaybeMember (removeMaxElement t)) (t :: NoDupes Int)
--
-- >>> removeMaxElement (fromList [2,3,5,7,4,1])
-- (Just 7,Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 5 (Node 4 Leaf Leaf) Leaf)))
--
-- >>> removeMaxElement Leaf
-- (Nothing,Leaf)
--
removeMinElement :: (Ord a) => BinaryTree a -> (Maybe a, BinaryTree a)
removeMinElement Leaf = (Nothing, Leaf)
removeMinElement (Node x Leaf Leaf) = (Just x, Leaf)
removeMinElement (Node x left right) = (minElement, Node x newLeft right)
    where (minElement, newLeft) = removeMinElement left


-- | merges two trees where all the elements in right are greater than
-- all the elements in the left.
--
-- >>> (fromList [2,1,3]) `mergeTrees` (fromList [5,4,6]) :: BinaryTree Int
-- Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 5 Leaf (Node 6 Leaf Leaf))
--
mergeTrees :: (Ord a) => BinaryTree a -> BinaryTree a -> BinaryTree a
mergeTrees Leaf t = t
mergeTrees left right = case removeMinElement right of
    (Nothing, _)        -> left
    (Just x, newRight)  -> Node x left newRight
