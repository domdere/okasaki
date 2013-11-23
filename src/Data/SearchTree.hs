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

module Data.SearchTree
    (   SearchTree(..)
    ,   fromList
    ) where

import Data.Set

-- $setup
-- >>> import Data.SearchTree.Binary
--
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
--
-- >>> newtype TestSearchTree a = TestSearchTree (BinaryTree a) deriving (Show, Eq)
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (TestSearchTree a) where arbitrary = (TestSearchTree . fromList) <$> arbitrary
--

class SearchTree t where
    emptyTree :: t a
    treeInsert :: (Ord a) => a -> t a -> t a
    treeInsertIfMissing :: (Ord a) => a -> t a -> t a
    treeMember :: (Ord a) => a -> t a -> Bool
    removedFromTree :: (Ord a) => a -> t a -> t a

-- | fromList converts a list of orderable elements into a BinaryTree containing those elements
--
-- >>> fromList [3,7,10,8,4,2,1] :: BinaryTree Int
-- Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 7 (Node 4 Leaf Leaf) (Node 10 (Node 8 Leaf Leaf) Leaf))
--
-- >>> (fromList []) :: BinaryTree Int
-- Leaf
--
-- >>> (fromList [1]) :: BinaryTree Int
-- Node 1 Leaf Leaf
--
fromList :: (SearchTree t, Ord a) => [a] -> t a
fromList = foldl (flip treeInsert) emptyTree

