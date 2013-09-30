module Data.Set.BinaryTree where

import Data.BinaryTree
import Data.Set

newtype BinaryTreeSet a = BinaryTreeSet (BinaryTree a) deriving (Show, Eq)


