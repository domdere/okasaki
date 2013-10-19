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

data BlackNonLeaf childNodeType a = BlackNode a (RedBlackTree childNodeType a) (RedBlackTree childNodeType a) deriving (Show, Eq)

-- | This type will enforce Invariant 1, Red nodes have only black children
--
-- it also enforces Invariant 2
-- both black children must be of the same type, and black nodes of different types have different depths
--
data RedNode blacktype a = RedNode a (blacktype a) (blacktype a) deriving (Show, Eq)

data RedBlackTree blacktype a =
        Red (RedNode blacktype a)
    |   Black (blacktype a) deriving (Show, Eq)

-- | An Insert Operation can lead to a tree of the same black height
-- or one with an incremented height
data SuccessfulRedBlackInsertResult blacktype a =
        Same (RedBlackTree blacktype a)
    |   Incremented (RedBlackTree (BlackNonLeaf blacktype) a) deriving (Show, Eq)

-- | 'ins' is the name used for the insert helper functions,
-- An insRed operation can result in a situation where Invariant 1 is
-- broken, the root node could be a Red node with a Red Child.
-- It inserts new elements as Red nodes and bubbles them up to the
-- top, hence insRed operations leave the black height unchanged.
--
data InsRedResult blacktype a =
        RedLeft a (RedNode blacktype a) (blacktype a)
    |   RedRight a (blacktype a) (RedNode blacktype a)
    |   Good (RedNode blacktype a) deriving (Show, Eq)

data InsRedBlackResult blacktype a =
        RedBlackLeft a (RedNode blacktype a) (blacktype a)
    |   RedBlackRight a (blacktype a) (RedNode blacktype a)
    |   GoodTree (RedBlackTree blacktype a) deriving (Show, Eq)

-- Classes and Instances

class BlackNode b where
    blackHeight :: b a -> Int
    insBlack :: (Ord a) => Bool -> a -> b a -> RedBlackTree b a

-- | BlackLeaf is the simplest instance of BlackNode
--
-- >>> blackHeight (BlackLeaf :: BlackLeaf Int)
-- 0
--
-- >>> insBlack 234 BlackLeaf
-- Red (RedNode 234 BlackLeaf BlackLeaf)
--
instance BlackNode BlackLeaf where
    blackHeight _ = 0
    insBlack _ x BlackLeaf = Red $ RedNode x BlackLeaf BlackLeaf

instance (BlackNode a) => BlackNode (BlackNonLeaf a) where
    blackHeight (BlackNode _ l _ ) = 1 + treeBlackHeight l

    insBlack dupes x n@(BlackNode y left right)
        | (x == y) && not dupes = Black n
        | x < y                 = lbalance y (insRedBlackTree dupes x left) right
        | otherwise             = rbalance y left (insRedBlackTree dupes x right)

-- Tree Functions

empty' :: RedBlackTree BlackLeaf a
empty' = Black BlackLeaf

-- | Inserts an element into the RedBlack Tree
--
--insertRB :: (Ord a)
--insertRB

-- Helper Functions

-- | returns the black height for a Red Node.
--
blackHeightRed :: (BlackNode b) => RedNode b a -> Int
blackHeightRed (RedNode _ l _) = blackHeight l

-- | returns the black heaight for a redblack tree
--
treeBlackHeight :: (BlackNode b) => RedBlackTree b a -> Int
treeBlackHeight (Black n)   = blackHeight n
treeBlackHeight (Red n)     = blackHeightRed n

-- | Returns a singleton tree, this isnt for external use but only for use
-- in inserting an element, it produces a tree witha red, when all externally
-- used instances should have black root nodes in their final state.
--
-- >>> singletonTree 1
-- Red (RedNode 1 BlackLeaf BlackLeaf)
--
singletonTree :: a -> RedBlackTree BlackLeaf a
singletonTree x = Red $ RedNode x BlackLeaf BlackLeaf

-- | inserts an Element into a Red Node.
-- If bool is true it will add the element even if its already there,
-- allowing dupes.
insRed :: (BlackNode b, Ord a) => Bool -> a -> RedNode b a -> InsRedResult b a
insRed dupes x t@(RedNode y left right)
    | (x == y) && not dupes = Good t
    | x < y                 = case insBlack dupes x left of
        Red newLeft     -> RedLeft y newLeft right
        Black newLeft   -> Good $ RedNode y newLeft right
    | otherwise             = case insBlack dupes x right of
        Red newRight    -> RedRight y left newRight
        Black newRight  -> Good $ RedNode y left newRight

insRedBlackTree :: (BlackNode b, Ord a) => Bool -> a -> RedBlackTree b a -> InsRedBlackResult b a
insRedBlackTree dupes x (Black blackNode)   = GoodTree $ insBlack dupes x blackNode
insRedBlackTree dupes x (Red redNode)       = case insRed dupes x redNode of
    RedLeft x' left right   -> RedBlackLeft x' left right
    RedRight x' left right  -> RedBlackRight x' left right
    Good goodRed            -> GoodTree $ Red goodRed

-- | during insBlack ops we may need to rebalance the tree,
-- breakages of invariant 1 bubble up the tree until they get to
-- a black node and either get rebalanced here or in `rbalance`
-- it gets called when trying to form a black node, and takes
-- the element at the intended black node, the possibly broken left child
-- and the good right child.
--
lbalance :: a -> InsRedBlackResult b a -> RedBlackTree b a -> RedBlackTree b a
lbalance x (GoodTree left) right        = Black $ BlackNode x left right
-- Double check this line....
lbalance z (RedBlackLeft x a r) d   = 

