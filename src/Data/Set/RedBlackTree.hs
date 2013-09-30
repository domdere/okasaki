module Data.Set.RedBlackTree where

-- | From Chapter 3.3 of Purely Functional Data Structures
-- "A Red-Black Tree is a binary search tree in which every node is coloured
-- either Red or Black."
--
-- Every Red Black tree satisfies two invariants:
--
--  1)  No Red node has a Red Child
--  2)  Every path from the root to an empty node contains the same
--      number of black nodes

data Colour = Red | Black deriving (Show, Eq)

-- | sometimes internally the tree will spend some time in a non-balanced state
-- where Invariant 1 is broken.
--
-- Invariant 2 should be satisfied at all times however
--
-- | This type will enforce Invariant 1
data RedNode a = RedNode a (BlackNode a) (BlackNode a) deriving (Show, Eq)

newtype RedBlackNode a = RBNode (Either (RedNode a) (BlackNode a)) deriving (Show, Eq)

data BlackNode a = BlackNode a (RBNode a) (RBNode a) deriving (Show, Eq)

data RedBlackTree a =
        EmptyRedBlack
    |   Node (Either (RedNode a) (BlackNode a)) deriving (Show, Eq)

-- | Balance an Unbalanced Red-Black Tree
-- The short version of what this function does is take 2 balanced
-- nodes, the element to be stored at the node at which they will be
-- fused and the intended colour of the node.
balance :: Colour -> a -> RBNode a -> RBNode a -> RBNode a
balance EmptyUnbalanced = EmptyRedBlack
