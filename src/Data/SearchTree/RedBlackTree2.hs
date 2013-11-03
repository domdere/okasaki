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
    RBTree :: RBNode Black h a -> RBTree a

deriving instance (Show a) => Show (RBTree a)

data Direction = L | R deriving (Show, Eq)

data BalanceState = Good | Broken deriving (Show, Eq)

--data NodeFocus :: BalanceState -> Colour -> Height -> * -> * where
--    GoodNode :: RBNode c h a -> NodeFocus Good c h a
--    BrokenRedLeftRedChild :: a -> RBNode Red h a -> RBNode Black h a -> NodeFocus Broken Red h a
--    BrokenRedRightRedChild :: a -> RBNode Black h a -> RBNode Red h a -> NodeFocus Broken Red h a
--    LeftHeightLow :: a -> RBNode cl h a -> RBNode cr (Inc h) a -> NodeFocus Broken Black (Inc (Inc h)) a
--    RightHeightLow :: a -> RBNode cl (Inc h) a -> RBNode cr h a -> NodeFocus Broken Black (Inc (Inc h)) a

--deriving instance (Show a) => Show (NodeFocus b c h a)

data ZipperContext :: BalanceState -> Height -> * -> * where
    GoodRedParentContext :: RBNode Black h a -> Direction -> RBNode Red h a -> RBNode Black h a -> ZipperContext Good h a
    BrokenRedParentRedChildContext :: RBNode Red h a -> Direction -> RBNode Red h a -> RBNode Black h a -> ZipperContext Broken h a
    BrokenRedParentHeightContext :: RBNode Black h a -> Direction -> RBNode Red h a -> RBNode Black (Inc h) a -> ZipperContext Broken (Inc h) a
    GoodBlackParentContext :: RBNode c h a -> Direction -> RBNode Black (Inc h) a -> RBNode cs h a -> ZipperContext Good h a
    BrokenBlackParentContext :: RBNode c h a -> Direction -> RBNode Black (Inc (Inc h)) a -> RBNode cs (Inc h) a -> ZipperContext Broken (Inc h) a

deriving instance (Show a) => Show (ZipperContext b h a)

data ZipperContextList :: BalanceState -> * -> * where
    Top :: RBNode Black h a -> ZipperContextList Good a
    (:.) :: ZipperContext b h a -> ZipperContextList Good a -> ZipperContextList b a

deriving instance (Show a) => Show (ZipperContextList b a)

data RBZipper :: BalanceState -> * -> * where
    RBZipper :: ZipperContextList b a -> RBZipper b a

deriving instance (Show a) => Show (RBZipper b a)

data RBZipperResult b a =
        New (RBZipper b a)
    |   Same (RBZipper b a) deriving (Show)

data RBRemoveMinResult a =
        Balanced (RBZipper Good a)
    |   Unbalanced (RBZipper Broken a) deriving (Show)

-- Zipper Functions

toZipper :: RBTree a -> RBZipper Good a
toZipper (RBTree t) = RBZipper (Top t)

goLeft :: RBZipper Good a -> RBZipperResult Good a
goLeft z@(RBZipper (Top Leaf))                                                      = Same z
goLeft z@(RBZipper ((GoodBlackParentContext Leaf _ _ _) :. _))                      = Same z
goLeft z@(RBZipper ((GoodRedParentContext Leaf _ _ _) :. _))                        = Same z
goLeft (RBZipper l@(Top p@(BlackNode _ left s)))                                    = New $ RBZipper $ (GoodBlackParentContext left L p s) :. l
goLeft (RBZipper l@((GoodBlackParentContext p@(BlackNode _ left s) _ _ _) :. _))    = New $ RBZipper $ (GoodBlackParentContext left L p s) :. l
goLeft (RBZipper l@((GoodBlackParentContext p@(RedNode _ left s) _ _ _) :. _))      = New $ RBZipper $ (GoodRedParentContext left L p s) :. l
goLeft (RBZipper l@((GoodRedParentContext p@(BlackNode _ left s) _ _ _) :. _))      = New $ RBZipper $ (GoodBlackParentContext left L p s) :. l

goRight :: RBZipper Good a -> RBZipperResult Good a
goRight z@(RBZipper (Top Leaf))                                                     = Same z
goRight z@(RBZipper ((GoodBlackParentContext Leaf _ _ _) :. _))                     = Same z
goRight z@(RBZipper ((GoodRedParentContext Leaf _ _ _) :. _))                       = Same z
goRight (RBZipper l@(Top p@(BlackNode _ s right)))                                  = New $ RBZipper $ (GoodBlackParentContext right R p s) :. l
goRight (RBZipper l@((GoodBlackParentContext p@(BlackNode _ s right) _ _ _) :. _))  = New $ RBZipper $ (GoodBlackParentContext right R p s) :. l
goRight (RBZipper l@((GoodBlackParentContext p@(RedNode _ s right) _ _ _) :. _))    = New $ RBZipper $ (GoodRedParentContext right R p s) :. l
goRight (RBZipper l@((GoodRedParentContext p@(BlackNode _ s right) _ _ _) :. _))    = New $ RBZipper $ (GoodBlackParentContext right R p s) :. l

goUp :: RBZipper Good a -> RBZipperResult Good a
goUp z@(RBZipper (Top {}))  = Same z
goUp (RBZipper (_ :. l))    = New $ RBZipper l

unwrapResult :: RBZipperResult b a -> RBZipper b a
unwrapResult (Same t)   = t
unwrapResult (New t)    = t

zipSearch :: (Ord a) => a -> RBZipper Good a -> RBZipper Good a
zipSearch _ z@(RBZipper (Top Leaf))                                    = z
zipSearch _ z@(RBZipper ((GoodBlackParentContext Leaf _ _ _) :. _))    = z
zipSearch _ z@(RBZipper ((GoodRedParentContext Leaf _ _ _) :. _))      = z
zipSearch x z@(RBZipper (Top (BlackNode y _ _)))
    | x == y    = z
    | otherwise = zipSearch x . unwrapResult $ (if x < y then goLeft else goRight) z
zipSearch x z@(RBZipper ((GoodBlackParentContext (BlackNode y _ _) _ _ _) :. _))
    | x == y = z
    | otherwise = zipSearch x . unwrapResult $ (if x < y then goLeft else goRight) z
zipSearch x z@(RBZipper ((GoodBlackParentContext (RedNode y _ _) _ _ _) :. _))
    | x == y = z
    | otherwise = zipSearch x . unwrapResult $ (if x < y then goLeft else goRight) z
zipSearch x z@(RBZipper ((GoodRedParentContext (BlackNode y _ _) _ _ _) :. _))
    | x == y = z
    | otherwise = zipSearch x . unwrapResult $ (if x < y then goLeft else goRight) z

zipRemoveMin :: RBZipper Good a -> Maybe (a, RBRemoveMinResult a)
zipRemoveMin (RBZipper (Top Leaf)) = Nothing
zipRemoveMin (RBZipper ((GoodBlackParentContext Leaf _ _ _) :. _)) = Nothing
zipRemoveMin (RBZipper ((GoodRedParentContext Leaf _ _ _) :. _)) = Nothing
--zipRemoveMin (RBZipper ((GoodRedParentContext (BlackNode x Leaf s@(RedNode y sl sr)) _ _ _) :. l)) = Just (x, Unbalanced $ RBZipper $ BrokenBlackParentContext Leaf)
