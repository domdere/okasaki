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

