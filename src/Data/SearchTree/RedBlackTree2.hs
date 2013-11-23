{-# LANGUAGE GADTs, KindSignatures, DataKinds, StandaloneDeriving #-}
module Data.SearchTree.RedBlackTree2 where

import Data.SearchTree

import Data.Function

-- | WIP: I've been looking at Red Black trees for a while now, trying to figure out
-- how to best write up the type so that it enforces the type invariant and
-- is also elegant to code.  I'm confident the code below works well to enforce the
-- invariants with the Type System, however as you can tell from looking at
-- `balanceBlackHeight`, it doesnt really have the elegance yet.  I'll come back to this
-- later with a fresh perspective.

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

-- | This data type covers all the result of search functions that return either
-- Leaf nodes or nodes with leaf children.
data FindLeafResult a where
    LeafResult :: ZipperCrumbList c Zero a -> FindLeafResult a
    BlackResult :: a -> RBNode cr Zero a -> ZipperCrumbList c (Inc Zero) a -> FindLeafResult a
    RedResult :: RBNode Red Zero a -> ZipperCrumbList Black Zero a -> FindLeafResult a

-- SearchTree Functions

-- removedFromTree' :: (Ord a) => a -> RBTree a -> RBTree a

-- Helper Functions

isLeaf :: RBNode c h a -> Bool
isLeaf Leaf = True
isLeaf _    = False

-- Zipper Functions

toZipper :: RBTree a -> RBZipper a
toZipper (RBTree node) = RBZipperBlack node Nil

isAtLeaf :: RBZipper a -> Bool
isAtLeaf (RBZipperRed {})       = False
isAtLeaf (RBZipperBlack n _)    = isLeaf n

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

unzipTree :: RBZipper a -> RBTree a
unzipTree (RBZipperRed redRoot Nil) = RBTree $ paintRedNodeBlack redRoot
unzipTree (RBZipperBlack root Nil) = RBTree root
unzipTree z = (unzipTree . goUp) z

getNodeValue :: RBZipper a -> Maybe a
getNodeValue (RBZipperBlack Leaf _)                 = Nothing
getNodeValue (RBZipperBlack (BlackNode y _ _) _)    = Just y
getNodeValue (RBZipperRed (RedNode y _ _) _)        = Just y

setNodeValue :: a -> RBZipper a -> RBZipper a
setNodeValue _ z@(RBZipperBlack Leaf _) = z
setNodeValue x (RBZipperBlack (BlackNode _ l r) crumbs) = RBZipperBlack (BlackNode x l r) crumbs
setNodeValue x (RBZipperRed (RedNode _ l r) crumbs) = RBZipperRed (RedNode x l r) crumbs

-- | Finds the given element and removes it from the tree, replacing it with the min
-- element of the right child
searchDownAndRemove :: (Ord a) => a -> RBZipper a -> RBZipper a

goDirection :: Direction -> RBZipper a -> RBZipper a
goDirection L = goLeft
goDirection R = goRight

assignDirection :: (Ord a) => a -> a -> Direction
assignDirection x y = if x < y then L else R

zipperSearch :: (Ord a) => a -> RBZipper a -> RBZipper a
zipperSearch x z = case getNodeValue z of
    Nothing -> z
    Just y  -> zipperSearch x $ goDirection (assignDirection x y) z

findMin :: RBZipper a -> FindLeafResult a
findMin (RBZipperBlack Leaf l)                      = LeafResult l
findMin (RBZipperBlack (BlackNode x Leaf right) l)  = BlackResult x right l
findMin (RBZipperRed n@(RedNode _ Leaf _) l)        = RedResult n l
findMin z                                           = (findMin . goLeft) z

findMax :: RBZipper a -> FindLeafResult a
findMax (RBZipperBlack Leaf l)                      = LeafResult l
findMax (RBZipperBlack (BlackNode x left Leaf) l)   = BlackResult x left l
findMax (RBZipperRed n@(RedNode _ _ Leaf) l)        = RedResult n l
findMax z                                           = (findMax . goRight) z

findLeafAndRemove :: (RBZipper a -> FindLeafResult a) -> RBZipper a -> RBZipper a
findLeafAndRemove findFunction z = case findFunction z of
    LeafResult _ -> z
    RedResult (RedNode _ Leaf Leaf) l -> RBZipperBlack Leaf l
    BlackResult _ (RedNode x left right) l -> RBZipperBlack (BlackNode x left right) l
    BlackResult _ Leaf l -> balanceBlackHeight Leaf l

findAndRemove :: (Ord a) => (RBZipper a -> RBZipper a) -> RBZipper a -> RBZipper a
findAndRemove findFunc = removeFocus . findFunc
    where
        findReplacement :: RBZipper a -> FindLeafResult a
        findReplacement z = if (goRight z) `isAtLeaf` then (findMax . goLeft) z else (findMin . goRight) z
        removeFocus :: RBZipper a -> RBZipper a
        removeFocus z@(RBZipperBlack Leaf _) = z
        removeFocus z = case findReplacement z of
            LeafResult _ -> z

paintRedNodeBlack :: RBNode Red h a -> RBNode Black (Inc h) a
paintRedNodeBlack (RedNode x l r) = BlackNode x l r

balanceBlackHeight :: RBNode Black h a -> ZipperCrumbList c (Inc h) a -> RBZipper a
balanceBlackHeight newRoot Nil = RBZipperBlack newRoot Nil
-- balanceBlackHeight _ ((BlackCrumb R _ Leaf) ::. _)
--balanceBlackHeight _ ((BlackCrumb R _ (BlackNode _ Leaf _)) ::. _) = error "Inaccessible"
--balanceBlackHeight _ ((BlackCrumb R _ (BlackNode _ (BlackNode _ _ _) Leaf)) ::. _) = error "Inaccessible"
balanceBlackHeight n ((BlackCrumb direction px s@(RedNode {})) ::. l)   = removalBalanceCase2 direction px n s l
balanceBlackHeight n ((BlackCrumb direction px (BlackNode sx sl@(BlackNode {}) sr@(BlackNode {}))) ::. l)   = removalBalanceCase3 direction px n sx sl sr l
balanceBlackHeight n ((BlackCrumb direction px (BlackNode sx Leaf Leaf)) ::. l)                             = removalBalanceCase3 direction px n sx Leaf Leaf l
balanceBlackHeight n ((RedCrumb direction px (BlackNode sx sl@(BlackNode {}) sr@(BlackNode {}))) :. l)      = removalBalanceCase4 direction px n sx sl sr l
balanceBlackHeight n ((RedCrumb R px (BlackNode sx sl@(BlackNode {}) sr@(RedNode {}))) :. l)                = balanceBlackHeight n ((RedCrumb R px (removalBalanceCase5Right sx sl sr)) :. l)
balanceBlackHeight n ((RedCrumb R px (BlackNode sx Leaf sr@(RedNode {}))) :. l)                             = balanceBlackHeight n ((RedCrumb R px (removalBalanceCase5Right sx Leaf sr)) :. l)
balanceBlackHeight n ((RedCrumb L px (BlackNode sx sl@(RedNode {}) Leaf)) :. l)                             = balanceBlackHeight n ((RedCrumb L px (removalBalanceCase5Left sx sl Leaf)) :. l)
balanceBlackHeight n ((BlackCrumb R px (BlackNode sx sl@(BlackNode {}) sr@(RedNode {}))) ::. l)             = balanceBlackHeight n ((BlackCrumb R px (removalBalanceCase5Right sx sl sr)) ::. l)
balanceBlackHeight n ((BlackCrumb R px (BlackNode sx Leaf sr@(RedNode {}))) ::. l)                          = balanceBlackHeight n ((BlackCrumb R px (removalBalanceCase5Right sx Leaf sr)) ::. l)
balanceBlackHeight n ((BlackCrumb L px (BlackNode sx sl@(RedNode {}) sr@(BlackNode {}))) ::. l)             = balanceBlackHeight n ((BlackCrumb L px (removalBalanceCase5Left sx sl sr)) ::. l)
balanceBlackHeight n ((BlackCrumb L px (BlackNode sx sl@(RedNode {}) Leaf)) ::. l)                          = balanceBlackHeight n ((BlackCrumb L px (removalBalanceCase5Left sx sl Leaf)) ::. l)
balanceBlackHeight n ((BlackCrumb R px (BlackNode sx sl@(RedNode {}) sr)) ::. l)                            = removalBalanceCase6BlackRight px n sx sl sr l
balanceBlackHeight n ((BlackCrumb L px (BlackNode sx sl sr@(RedNode {}))) ::. l)                            = removalBalanceCase6BlackLeft px n sx sl sr l
balanceBlackHeight n ((RedCrumb R px (BlackNode sx sl@(RedNode {}) sr)) :. l)                               = removalBalanceCase6RedRight px n sx sl sr l
balanceBlackHeight n ((RedCrumb L px (BlackNode sx sl sr@(RedNode {}))) :. l)                               = removalBalanceCase6RedLeft px n sx sl sr l
-- this is dirty but needs to be here, otherwise GHC warns about the following patterns not being handled, which are technically
-- impossible to construct, attempting to match the patterns given as is results in compiler errors (i.e the compiler should be able to
-- prove that the missing patterns are impossible but it doesnt (GHC 7.6.3))
-- hence the complete set of plausible patterns has actually been handled.
--      _ ((BlackCrumb R _ Leaf) ::. _)
--      _ ((BlackCrumb R _ (BlackNode _ (BlackNode _ _ _) Leaf)) ::. _)
--      _ ((BlackCrumb R _ (BlackNode _ Leaf (BlackNode _ _ _))) ::. _)
--      _ ((BlackCrumb L _ Leaf) ::. _)
--      _ ((BlackCrumb L _ (BlackNode _ (BlackNode _ _ _) Leaf)) ::. _)
--      _ ((BlackCrumb L _ (BlackNode _ Leaf (BlackNode _ _ _))) ::. _)
--
-- Seems to be a long outstanding GHC+GADTs bug, theres a ticket here:
-- http://ghc.haskell.org/trac/ghc/ticket/595
-- or more specifically this one:
-- http://ghc.haskell.org/trac/ghc/ticket/3927
-- due to be fixed in 7.8.1 (currently)
balanceBlackHeight _ _ = undefined

removalBalanceCase2 :: Direction -> a -> RBNode Black h a -> RBNode Red (Inc h) a -> ZipperCrumbList c (Inc (Inc h)) a -> RBZipper a
removalBalanceCase2 R px n (RedNode sx sl sr) crumbs    = balanceBlackHeight n ((RedCrumb R px sr) :. ((BlackCrumb R sx sl) ::. crumbs))
removalBalanceCase2 L px n (RedNode sx sl sr) crumbs    = balanceBlackHeight n ((RedCrumb L px sl) :. ((BlackCrumb L sx sr) ::. crumbs))

removalBalanceCase3 :: Direction -> a -> RBNode Black h a -> a -> RBNode Black h a -> RBNode Black h a -> ZipperCrumbList c (Inc (Inc h)) a -> RBZipper a
removalBalanceCase3 R px n sx sl sr crumbs  = goRight $ balanceBlackHeight (BlackNode px (RedNode sx sl sr) n) crumbs
removalBalanceCase3 L px n sx sl sr crumbs  = goLeft $ balanceBlackHeight (BlackNode px n (RedNode sx sl sr)) crumbs

removalBalanceCase4 :: Direction -> a -> RBNode Black h a -> a -> RBNode Black h a -> RBNode Black h a -> ZipperCrumbList c (Inc h) a -> RBZipper a
removalBalanceCase4 R px n sx sl sr crumbs = goRight $ RBZipperBlack (BlackNode px (RedNode sx sl sr) n) crumbs
removalBalanceCase4 L px n sx sl sr crumbs = goLeft $ RBZipperBlack (BlackNode px n (RedNode sx sl sr)) crumbs

removalBalanceCase5Right :: a -> RBNode Black h a -> RBNode Red h a -> RBNode Black (Inc h) a
removalBalanceCase5Right sx sl (RedNode srx srl srr) = BlackNode srx (RedNode sx sl srl) srr

removalBalanceCase5Left :: a -> RBNode Red h a -> RBNode Black h a -> RBNode Black (Inc h) a
removalBalanceCase5Left sx (RedNode slx sll slr) sr = BlackNode slx sll (RedNode sx slr sr)

removalBalanceCase6BlackRight :: a -> RBNode Black h a -> a -> RBNode Red h a -> RBNode csr h a -> ZipperCrumbList c (Inc (Inc h)) a -> RBZipper a
removalBalanceCase6BlackRight px n sx sl sr crumbs = goRight $ goRight $ RBZipperBlack (BlackNode sx (paintRedNodeBlack sl) (BlackNode px sr n)) crumbs

removalBalanceCase6BlackLeft :: a -> RBNode Black h a -> a -> RBNode csl h a -> RBNode Red h a -> ZipperCrumbList c (Inc (Inc h)) a -> RBZipper a
removalBalanceCase6BlackLeft px n sx sl sr crumbs = goLeft $ goLeft $ RBZipperBlack (BlackNode sx (BlackNode px n sl) (paintRedNodeBlack sr)) crumbs

removalBalanceCase6RedRight :: a -> RBNode Black h a -> a -> RBNode Red h a -> RBNode csr h a -> ZipperCrumbList Black (Inc h) a -> RBZipper a
removalBalanceCase6RedRight px n sx sl sr crumbs = goRight $ goRight $ RBZipperRed (RedNode sx (paintRedNodeBlack sl) (BlackNode px sr n)) crumbs

removalBalanceCase6RedLeft :: a -> RBNode Black h a -> a -> RBNode csl h a -> RBNode Red h a -> ZipperCrumbList Black (Inc h) a -> RBZipper a
removalBalanceCase6RedLeft px n sx sl sr crumbs = goLeft $ goLeft $ RBZipperRed (RedNode sx (BlackNode px n sl) (paintRedNodeBlack sr)) crumbs

