module Data.Set.BinaryTree where

import qualified Data.BinaryTree as BT
import Data.Set

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (BinaryTreeSet a) where arbitrary = (BinaryTreeSet . BT.fromList) <$> arbitrary

newtype BinaryTreeSet a = BinaryTreeSet 
    {   fromBinaryTreeSet :: BT.BinaryTree a
    } deriving (Show, Eq)

-- | Implement insert for Binary Tree again such that dupes are not added.
--
-- prop> (\x t -> ((==) =<< (insert x)) (x `insert` t)) (x :: Int) (t :: BinaryTreeSet Int)
--
-- >>> 2 `insert'` BT.Leaf
-- Node 2 Leaf Leaf
--
-- >>> 2 `insert'` (2 `insert'` BT.Leaf)
-- Node 2 Leaf Leaf
--
insert' :: (Ord a) => a -> BT.BinaryTree a -> BT.BinaryTree a
insert' x BT.Leaf = BT.Node x BT.Leaf BT.Leaf
insert' x t@(BT.Node x' left right)
    | x == x'   = t
    | x < x'    = BT.Node x' (x `insert'` left) right
    | otherwise = BT.Node x' left (x `insert'` right)

instance Set BinaryTreeSet where
    empty = BinaryTreeSet BT.Leaf

    insert x t = BinaryTreeSet $ x `insert'` fromBinaryTreeSet t

    remove x t = BinaryTreeSet $ x `BT.removedFrom` fromBinaryTreeSet t

    member x t = x `BT.member` fromBinaryTreeSet t
