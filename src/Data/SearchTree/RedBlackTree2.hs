{-# LANGUAGE GADTs, KindSignatures, DataKinds, StandaloneDeriving #-}
module Data.SearchTree.RedBlackTree2 where

import Data.SearchTree

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

-- The actual RBTree implmentation, using all of the classes and functions below.

-- Data types...

data Colour = Red | Black deriving (Show, Eq)

data Height = Zero | Inc Height deriving (Show, Eq)

data NonZeroHeight b = NonZeroHeight b deriving (Show, Eq)

data RBNode :: Colour -> Height -> * -> * where
    Leaf :: RBNode Black Zero a
    BlackNode :: a -> RBNode cl h a -> RBNode cr h a -> RBNode Black (Inc h) a
    RedNode :: a -> RBNode Black h a -> RBNode Black h a -> RBNode Red h a

deriving instance (Show a) => Show (RBNode c h a)

data RBTree a where
    RBTree :: RBNode c h a -> RBTree a

deriving instance (Show a) => Show (RBTree a)

data Direction = Left | Right deriving (Show, Eq)

data NodeFocus :: Colour -> Height -> * -> * where
    GoodNode :: RBNode c h a -> NodeFocus c h a
    BrokenRedLeftRedChild :: a -> RBNode Red h a -> RBNode Black h a -> NodeFocus Red h a
    BrokenRedRightRedChild :: a -> RBNode Black h a -> RBNode Red h a -> NodeFocus Red h a
    LeftHeightLow :: a -> RBNode cl h a -> RBNode cr (Inc h) a -> NodeFocus Black (Inc (Inc h)) a
    RightHeightLow :: a -> RBNode cl (Inc h) a -> RBNode cr h a -> NodeFocus Black (Inc (Inc h)) a

deriving instance (Show a) => Show (NodeFocus c h a)

data ZipperContext :: Height -> * -> * where
    RootContext :: NodeFocus c h a -> ZipperContext h a
    RedParentContext :: NodeFocus c h a -> Direction -> RBNode Red h a -> RBNode Black h a -> ZipperContext h a
    BlackParentContext :: NodeFocus c h a -> Direction -> RBNode Black (Inc h) a -> RBNode cs h a -> ZipperContext h a

deriving instance (Show a) => Show (ZipperContext h a)

data ZipperContextList :: Height -> * -> * where
    Nil :: ZipperContextList h a
    (:.) :: ZipperContext h a -> ZipperContextList h a -> ZipperContextList h a
    (::.) :: ZipperContext h a -> ZipperContextList h a -> ZipperContextList (Inc h) a

deriving instance (Show a) => Show (ZipperContextList h a)

data RBZipper a where
    RBZipper :: ZipperContextList h a -> RBZipper a

deriving instance (Show a) => Show (RBZipper a)

data RBZipperResult a =
        LowerDepth (RBZipper a)
    |   SameDepth (RBZipper a)
    |   Same (RBZipper a) deriving (Show)

-- Zipper Functions

toZipper :: RBTree a -> RBZipper a
toZipper (RBTree t) = RBZipper ((RootContext $ GoodNode t) :. Nil)

--toLeft :: RBZipper a -> RBZipperResult a

