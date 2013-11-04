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

data ZipperCrumb :: Colour -> Height -> * -> * where
    BlackCrumb :: Direction -> a -> RBNode cs h a -> ZipperCrumb Black (Inc h) a
    RedCrumb :: Direction -> a -> RBNode Black h a -> ZipperCrumb Red h a

deriving instance (Show a) => Show (ZipperCrumb c h a)

data ZipperCrumbList :: Colour -> Height -> * -> * where
    Nil :: ZipperCrumbList Black h a
    (:.) :: ZipperCrumb Red h a -> ZipperCrumbList Black h a -> ZipperCrumbList Red h a
    (::.) :: ZipperCrumb Black (Inc h) a -> ZipperCrumbList cp (Inc h) a -> ZipperCrumbList Black h a

deriving instance (Show a) => Show (ZipperCrumbList b h a)

data RBZipper a  where
    RBZipperRed :: RBNode Red h a -> ZipperCrumbList Black h a -> RBZipper a
    RBZipperBlack :: RBNode Black h a -> ZipperCrumbList c h a -> RBZipper a

deriving instance (Show a) => Show (RBZipper a)

data FindMinResult a where
    LeafResult :: ZipperCrumbList c Zero a -> FindMinResult a
    MinBlackResult :: a -> RBNode cr Zero a -> ZipperCrumbList c (Inc Zero) a -> FindMinResult a
    MinRedResult :: RBNode Red Zero a -> ZipperCrumbList Black Zero a -> FindMinResult a

-- Zipper Functions

toZipper :: RBTree a -> RBZipper a
toZipper (RBTree node) = RBZipperBlack node Nil

goLeft :: RBZipper a -> RBZipper a
goLeft z@(RBZipperBlack Leaf _)                                     = z
goLeft (RBZipperBlack (BlackNode x left@(Leaf) right) l)            = RBZipperBlack left ((BlackCrumb L x right) ::. l)
goLeft (RBZipperBlack (BlackNode x left@(BlackNode {}) right) l)    = RBZipperBlack left ((BlackCrumb L x right) ::. l)
goLeft (RBZipperBlack (BlackNode x left@(RedNode {}) right) l)      = RBZipperRed left ((BlackCrumb L x right) ::. l)
goLeft (RBZipperRed (RedNode x left right) l)                       = RBZipperBlack left ((RedCrumb L x right) :. l)

goRight :: RBZipper a -> RBZipper a
goRight z@(RBZipperBlack Leaf _)                                    = z
goRight (RBZipperBlack (BlackNode x left right@(Leaf)) l)           = RBZipperBlack right ((BlackCrumb R x left) ::. l)
goRight (RBZipperBlack (BlackNode x left right@(BlackNode {})) l)   = RBZipperBlack right ((BlackCrumb R x left) ::. l)
goRight (RBZipperBlack (BlackNode x left right@(RedNode {})) l)     = RBZipperRed right ((BlackCrumb R x left) ::. l)
goRight (RBZipperRed (RedNode x left right) l)                      = RBZipperBlack right ((RedCrumb R x left) :. l)

goUp :: RBZipper a -> RBZipper a
goUp z@(RBZipperBlack _ Nil)                        = z
goUp z@(RBZipperRed _ Nil)                          = z
goUp (RBZipperBlack n ((BlackCrumb L x s) ::. l))   = RBZipperBlack (BlackNode x n s) l
goUp (RBZipperBlack n ((BlackCrumb R x s) ::. l))   = RBZipperBlack (BlackNode x s n) l
goUp (RBZipperRed n ((BlackCrumb L x s) ::. l))     = RBZipperBlack (BlackNode x n s) l
goUp (RBZipperRed n ((BlackCrumb R x s) ::. l))     = RBZipperBlack (BlackNode x s n) l
goUp (RBZipperBlack n ((RedCrumb L x s) :. l))      = RBZipperRed (RedNode x n s) l
goUp (RBZipperBlack n ((RedCrumb R x s) :. l))      = RBZipperRed (RedNode x s n) l

getNodeValue :: RBZipper a -> Maybe a
getNodeValue (RBZipperBlack Leaf _)                 = Nothing
getNodeValue (RBZipperBlack (BlackNode y _ _) _)    = Just y
getNodeValue (RBZipperRed (RedNode y _ _) _)        = Just y

goDirection :: Direction -> RBZipper a -> RBZipper a
goDirection L = goLeft
goDirection R = goRight

assignDirection :: (Ord a) => a -> a -> Direction
assignDirection x y = if x < y then L else R

zipperSearch :: (Ord a) => a -> RBZipper a -> RBZipper a
zipperSearch x z = case getNodeValue z of
    Nothing -> z
    Just y  -> zipperSearch x $ goDirection (assignDirection x y) z

findMin :: RBZipper a -> FindMinResult a
findMin (RBZipperBlack Leaf l)                      = LeafResult l
findMin (RBZipperBlack (BlackNode x Leaf right) l)  = MinBlackResult x right l
findMin (RBZipperRed n@(RedNode _ Leaf _) l)        = MinRedResult n l
findMin z                                           = (findMin . goLeft) z

removeMin :: RBZipper a -> RBZipper a
removeMin z = case findMin z of
    LeafResult _ -> z
    MinRedResult (RedNode _ Leaf Leaf) l -> RBZipperBlack Leaf l
    MinBlackResult _ (RedNode x left right) l -> RBZipperBlack (BlackNode x left right) l
    MinBlackResult _ Leaf l -> balanceBlackHeight Leaf l

balanceBlackHeight :: RBNode Black h a -> ZipperCrumbList c (Inc h) a -> RBZipper a
balanceBlackHeight newRoot Nil = RBZipperBlack newRoot Nil
balanceBlackHeight n ((BlackCrumb direction px s@(RedNode {})) ::. l) = removalBalanceCase2 direction px n s l

removalBalanceCase2 :: Direction -> a -> RBNode Black h a -> RBNode Red (Inc h) a -> ZipperCrumbList c (Inc h) a -> RBZipper a
removalBalanceCase2 = error "TODO:"
