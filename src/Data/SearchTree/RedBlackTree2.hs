module Data.SearchTree.RedBlackTree2 where

import Data.Function

-- | From Chapter 3.3 of Purely Functional Data Structures
-- "A Red-Black Tree is a binary search tree in which every node is coloured
-- either Red or Black."
--
-- Every Red Black tree satisfies two invariants:
--
--  1.  No Red node has a Red Child
--  2.  Every path from the root to an empty node contains the same
--      number of black nodes
--
-- During Inserts, Invariant 1 is broken until rebalancing occurs,
-- During Deletes, Invariant 2 is broken until rebalancing occurs.

-- | this data type will help enforce Invariant 2
-- BlackLeaf and BlackNonLeaf will encompass the differences that are entangled
-- between a black node and its black height/depth, i.e
-- a black node of zero depth is a leaf with no element,
-- while a black node of non zero depth contains an element.
--

-- Data types...

data BlackLeaf a = BlackLeaf deriving (Show, Eq)

data BlackNonLeaf childNodeType a = BlackNonLeaf a (RedBlackTree childNodeType a) (RedBlackTree childNodeType a) deriving (Show, Eq)

-- | This type will enforce Invariant 1, Red nodes have only black children
--
-- it also enforces Invariant 2
-- both black children must be of the same type, and black nodes of different types have different depths
--
data RedNode blacktype a = RedNode a (blacktype a) (blacktype a) deriving (Show, Eq)

-- |
data RedBlackTree blacktype a =
        Red (RedNode blacktype a)
    |   Black (blacktype a) deriving (Show, Eq)

-- Classes and Instances

class BlackHeight b where
    blackHeight :: b a -> Int

instance BlackHeight BlackLeaf where
    blackHeight _ = 0

instance (BlackHeight a) => BlackHeight (BlackNonLeaf a) where
    blackHeight (BlackNonLeaf _ l _ ) = 1 + blackHeight l

instance (BlackHeight a) => BlackHeight (RedNode a) where
    blackHeight (RedNode _ l _) = blackHeight l

instance (BlackHeight a) => BlackHeight (RedBlackTree a) where
    blackHeight (Red red)       = blackHeight red
    blackHeight (Black black)   = blackHeight black

-- Tree Functions

empty' :: RedBlackTree BlackLeaf a
empty' = Black BlackLeaf

-- Helper Functions
