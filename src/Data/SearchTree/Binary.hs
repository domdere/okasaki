{-
Copyright (c) 2013 Dom De Re

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

module Data.SearchTree.Binary
    (   BinaryTree(..)
    ) where

import Data.SearchTree

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
--
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (BinaryTree a) where arbitrary = fromList <$> arbitrary

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
-- Node 1 Leaf (Node 3 Leaf Leaf)
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
