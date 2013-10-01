module Data.SearchTree.RedBlackTree where

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

newtype RedBlackTree a = RBNode (Either (RedNode a) (BlackNode a)) deriving (Show, Eq)

-- | the Leaf technically has no colour associated with it...
data BlackNode a = Leaf | BlackNode a (RedBlackTree a) (RedBlackTree a) deriving (Show, Eq)

-- | At times during the insert, there may exist violations of Invariant 1 at the roots of the subtrees considered,
-- they will be corrected before the insert operation continues. these are the allowed broken states..
data BrokenRBTree a =
        RedLeftRedChild a (RedNode a) (BlackNode a)
    |   RedRightRedChild a (BlackNode a) (RedNode a) deriving (Show, Eq)

-- Helpers

-- | Balance an Unbalanced Red-Black Tree
-- The short version of what this function does is take the element to be stored at the node,
-- the desired colour of the node, the unbalanced left node and a potentially
-- balanced right node, the element to be stored at the node at which they will be
-- and then returns a new node/tree which is balanced at the root also
-- (but Invariant 1 is satisfied for its children trees)
leftBalance :: a -> BrokenRBTree a -> RedBlackTree a -> RedBlackTree a
leftBalance z (RedLeftRedChild y (RedNode x a b) c) d   = RBNode $ Left $ RedNode y (BlackNode x (wrapBlack a) (wrapBlack b)) (BlackNode z (wrapBlack c) d)
leftBalance z (RedRightRedChild x a (RedNode y b c)) d  = RBNode $ Left $ RedNode y (BlackNode x (wrapBlack a) (wrapBlack b)) (BlackNode z (wrapBlack c) d)

-- | Balance an Unbalanced Red-Black Tree
-- The short version of what this function does is take the element to be stored at the node,
-- the desired colour of the node, the balanced left node and an
-- unbalanced right node, the element to be stored at the node at which they will be
-- and then returns a new node/tree which is balanced at the root also
-- (but Invariant 1 is satisfied for its children trees)
rightBalance :: a -> RedBlackTree a -> BrokenRBTree a -> RedBlackTree a
rightBalance x a (RedLeftRedChild z (RedNode y b c) d)   = RBNode $ Left $ RedNode y (BlackNode x a (wrapBlack b)) (BlackNode z (wrapBlack c) (wrapBlack d))
rightBalance x a (RedRightRedChild y b (RedNode z c d))  = RBNode $ Left $ RedNode y (BlackNode x a (wrapBlack b)) (BlackNode z (wrapBlack c) (wrapBlack d))


-- | Wraps a Black node up to make it a red black tree
--
wrapBlack :: BlackNode a -> RedBlackTree a
wrapBlack = RBNode . Right

-- | Wraps a Red node up to make it a red black tree
--
wrapRed :: RedNode a -> RedBlackTree a
wrapRed = RBNode . Left

-- | Inserts an element into a red-black tree, potentially leaving invariant 1 broken at the root
--
ins :: (Ord a) => a -> RedBlackTree a -> Either (BrokenRBTree a) (RedBlackTree a)
ins x (RBNode (Right Leaf)) = Right $ RBNode $ Left $ RedNode x Leaf Leaf
ins x (RBNode (Left (RedNode y left right)))
    | x < y     = case x `insBlack` left of
        RBNode (Right b)    -> Right $ RBNode $ Left $ RedNode y b right
        RBNode (Left b)     -> Left $ RedLeftRedChild y b right
    | otherwise = case x `insBlack` right of
        RBNode (Right b)    -> Right $ RBNode $ Left $ RedNode y left b
        RBNode (Left b)     -> Left $ RedRightRedChild y left b
ins x (RBNode (Right blackNode)) = Right $ x `insBlack` blackNode

-- | inserts an element into a black node
insBlack :: (Ord a) => a -> BlackNode a -> RedBlackTree a
insBlack x Leaf = RBNode $ Left $ RedNode x Leaf Leaf
insBlack x (BlackNode y left right)
    | x < y     = case x `ins` left of
        Left brokenLeft -> leftBalance y brokenLeft right
        Right a         -> RBNode $ Right $ BlackNode y a right
    | otherwise = case x `ins` right of
        Left brokenRight    -> rightBalance y left brokenRight
        Right a             -> RBNode $ Right $ BlackNode y left a

