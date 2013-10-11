module Data.SearchTree.RedBlackTree where

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

-- | This type will enforce Invariant 1
data RedBlackTree a =
        Red a (BlackNode a) (BlackNode a)
    |   Black (BlackNode a) deriving (Show, Eq)

data BlackNode a =
        Leaf
    |   Node a (RedBlackTree a) (RedBlackTree a) deriving (Show, Eq)

data RedNode a = RNode a (BlackNode a) (BlackNode a) deriving (Show, Eq)

-- | During inserts the 1st invariant can be broken and there can be
-- a red node with a red child,
-- this type represents the instances of the 1st broken invariant that
-- we will come across.
--
data InsertBrokenNode a =
        RedLeftRedChild a (RedNode a) (BlackNode a)
    |   RedRightRedChild a (BlackNode a) (RedNode a) deriving (Show, Eq)

-- | During deletes the 2nd invariant can be broken, when a black
-- node is deleted and there becomes a path from the root to a Leaf
-- node that has a different black count than the rest.  This type
-- marks this case.
--
data DeleteResult a =
        Broken (BlackNode a)
    |   Ok (RedBlackTree a) deriving (Show, Eq)

-- These will be the SearchTree instance functions

-- | The empty instance
--
empty' :: RedBlackTree a
empty' = Black Leaf

-- | insert an element into the tree
--
insert' :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
insert' x t = either fixInsertBrokenNode colourRootBlack $ insTree True x t

-- | inserts an element into the tree only if its not in the tree
-- already
--
insertIfMissing' :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
insertIfMissing' x t = either fixInsertBrokenNode colourRootBlack $ insTree False x t

-- | Checks for membership
--
member' :: (Ord a) => a -> RedBlackTree a -> Bool
x `member'` (Red y left right)
    | x == y            = True
    | otherwise         = (member' x . Black) $ if x < y then left else right
x `member'` (Black b)   = x `memberInBlack` b
    where
        memberInBlack :: (Ord a) => a -> BlackNode a -> Bool
        _ `memberInBlack` Leaf  = False
        x' `memberInBlack` (Node y left right)
            | x' == y           = True
            | otherwise         = member' x' $ if x' < y then left else right


-- Helpers

-- | Count the black nodes in a tree, technically the Leaves count as
-- Black but they wont be counted.
--
-- >>> countBlack (Black Leaf)
-- 0
--
countBlack :: RedBlackTree a -> Int
countBlack (Black Leaf)                 = 0
countBlack (Black (Node _ left right))  = 1 + ((+) `on` countBlack) left right
countBlack (Red _ left right)           = ((+) `on` (countBlack . Black)) left right

-- | Count the red nodes in a tree.
--
-- >>> countRed (Black Leaf)
-- 0
--
countRed :: RedBlackTree a -> Int
countRed (Black Leaf)                 = 0
countRed (Black (Node _ left right))  = ((+) `on` countRed) left right
countRed (Red _ left right)           = 1 + ((+) `on` (countRed . Black)) left right

-- | During Inserts the 1st invariant gets broken and
-- needs to be fixed as we merge the trees back up,
-- this function fuses together a broken left tree and a
-- balanced right tree, with a node with the given element.
--
leftBalance :: a -> InsertBrokenNode a -> RedBlackTree a -> RedBlackTree a
leftBalance z (RedLeftRedChild y (RNode x a b) c) d     = Red y (Node x (Black a) (Black b)) (Node z (Black c) d)
leftBalance z (RedRightRedChild x a (RNode y b c)) d    = Red y (Node x (Black a) (Black b)) (Node z (Black c) d)

-- | During Inserts the 1st invariant gets broken and
-- needs to be fixed as we merge the trees back up,
-- this function fuses together a balanced left tree and a
-- broken right tree, with a node with the given element.
--
rightBalance :: a -> RedBlackTree a -> InsertBrokenNode a -> RedBlackTree a
rightBalance x a (RedLeftRedChild z (RNode y b c) d)   = Red y (Node x a (Black b)) (Node z (Black c) (Black d))
rightBalance x a (RedRightRedChild y b (RNode z c d))  = Red y (Node x a (Black b)) (Node z (Black c) (Black d))

-- | wraps a RedNode up as a RedBlackTree a
--
wrapRed :: RedNode a -> RedBlackTree a
wrapRed (RNode x left right) = Red x left right

-- | insTree inserts an element into a red black tree, works recursively
-- and returns a tree that breaks Invariant 1 at the root,
-- effectively bubbling the broken Red Node up towards the root until
-- it can be rebalanced.
--
-- If the bool is true it will still add the element if it is already
-- in the tree.
--
insTree :: (Ord a) => Bool -> a -> RedBlackTree a -> Either (InsertBrokenNode a) (RedBlackTree a)
insTree addDupes x (Black blackNode) = Right $ insBlack addDupes x blackNode
insTree addDupes x t@(Red y left right)
    | x < y                     = case insBlack addDupes x left of
        Black b             -> Right $ Red y b right
        Red y' left' right' -> Left $ RedLeftRedChild y (RNode y' left' right') right
    | x == y && not addDupes    = Right t
    | otherwise                 = case insBlack addDupes x right of
        Black b             -> Right $ Red y left b
        Red y' left' right' -> Left $ RedRightRedChild y left (RNode y' left' right')

-- | inserts an element into a black node
--
-- If the Bool is true it will add dupes to the tree if the element already
-- exists in the tree.
--
insBlack :: (Ord a) => Bool -> a -> BlackNode a -> RedBlackTree a
insBlack _ x Leaf               = Red x Leaf Leaf
insBlack addDupes x t@(Node y left right)
    | x < y                     = case insTree addDupes x left of
        Left brokenLeft     -> leftBalance y brokenLeft right
        Right a             -> Black $ Node y a right
    | x == y && not addDupes    = Black t
    | otherwise                 = case insTree addDupes x right of
        Left brokenRight    -> rightBalance y left brokenRight
        Right a             -> Black $ Node y left a

-- | colours the root node black
--
colourRootBlack :: RedBlackTree a -> RedBlackTree a
colourRootBlack t@(Black _) = t
colourRootBlack (Red x left right) = Black $ (Node x `on` Black) left right

-- | fix broken tree
--
-- once the red node with the red child is bubbled to the top it can
-- be fixed by just colouring the node Black.
--
fixInsertBrokenNode :: InsertBrokenNode a -> RedBlackTree a
fixInsertBrokenNode (RedLeftRedChild x left right)  = Black $ Node x (wrapRed left) (Black right)
fixInsertBrokenNode (RedRightRedChild x left right) = Black $ Node x (Black left) (wrapRed right)

-- | removes the minimum element of a tree and returns
-- both the element removed and the resultant tree.
--
removeMin :: RedBlackTree a -> (Maybe a, DeleteResult a)
removeMin (Black Leaf) = (Nothing, Ok $ Black Leaf)
removeMin (Red x left s@(Node y (Black sl) (Black sr))) = case removeMin (Black left) of
    (Nothing, _) -> (Just x, Ok $ Black s)
    (x', Ok (Black left')) -> (x', Ok $ Red x left' s)
    (x', Broken n) -> (x', Ok $ Black $ Node x (Black n) (Red y sl sr))
removeMin (Red x left s@(Node y (Red slx sll slr) (Black sr))) = case removeMin (Black left) of
    (Nothing, _) -> (Just x, Ok $ Black s)
    (x', Ok (Black left')) -> (x', Ok $ Red x left' s)
    (x', Broken n) -> (x', Ok $ Red x n $ fixDeleteCase y (RNode slx sll slr) sr)
removeMin (Red x left s@(Node y sl (Red srx srl srr))) = case removeMin (Black left) of
    (Nothing, _) -> (Just x, Ok $ Black s)
    (x', Ok (Black left')) -> (x', Ok $ Red x left' s)
    (x', Broken n) -> (x', Ok $ Red y (Node x (Black n) sl) $ Node srx (Black srl) (Black srr))
removeMin (Black (Node x left s@(Red y sl sr))) = case removeMin left of
    (Nothing, _) -> (Just x, (Ok . colourRootBlack) s)
    (x', Ok left') -> (x', Ok $ Black $ Node x left' s)
    (x', Broken n) -> (x', Ok $ Black $ Node y (Red x n sl) (Black sr))
removeMin (Black (Node x left (Black s@(Node y (Black sl) (Black sr))))) = case removeMin left of
    (Nothing, _) -> (Just x, Broken s)
    (x', Ok left') -> (x', Ok $ Black $ Node x left' (Black s))
    (x', Broken n) -> (x', Ok $ Black $ Node x (Black n) (Red y sl sr))
removeMin (Black (Node x left (Black s@(Node sx (Red slx sll slr) (Black sr))))) = case removeMin left of
    (Nothing, _) -> (Just x, Broken s)
    (x', Ok left') -> (x', Ok $ Black $ Node x left' (Black s))
    (x', Broken n) -> (x', Ok $ Black $ Node x (Black n) (Black $ fixDeleteCase sx (RNode slx sll slr) sr))
removeMin (Black (Node x left (Black s@(Node y sl (Red srx srl srr))))) = case removeMin left of
    (Nothing, _) -> (Just x, Broken s)
    (x', Ok left') -> (x', Ok $ Black $ Node x left' (Black s))
    (x', Broken n) -> (x', Ok $ Black $ Node y (Black $ Node x (Black n) sl) $ Black $ Node srx (Black srl) (Black srr))
removeMin (Red _ _ Leaf) = error "Unbalanced Tree, Invariant 2 broken..."
removeMin (Black (Node _ _ (Black Leaf))) = error "Unbalanced Tree, Invariant 2 broken..."

-- | rebalances one of the delete cases see `removeMin` to see which one.
--
fixDeleteCase :: a -> RedNode a -> BlackNode a -> BlackNode a
fixDeleteCase s (RNode x sll slr) sr = Node x (Black sll) (Red s slr sr)
