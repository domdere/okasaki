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

data BlackNonLeaf childNodeType a = BlackNode a (RedOrBlackNode childNodeType a) (RedOrBlackNode childNodeType a) deriving (Show, Eq)

-- | This type will enforce Invariant 1, Red nodes have only black children
--
-- it also enforces Invariant 2
-- both black children must be of the same type, and black nodes of different types have different depths
--
data RedNode blacktype a = RedNode a (blacktype a) (blacktype a) deriving (Show, Eq)

data RedOrBlackNode blacktype a =
        Red (RedNode blacktype a)
    |   Black (blacktype a) deriving (Show, Eq)

-- | An Insert Operation can lead to a tree of the same black height
-- or one with an incremented height
data SuccessfulRedBlackInsertResult blacktype a =
        Same (blacktype a)
    |   Incremented ((BlackNonLeaf blacktype) a) deriving (Show, Eq)

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
    |   GoodTree (RedOrBlackNode blacktype a) deriving (Show, Eq)

data PossibleHeightIncResult blacktype a =
        SameHeight (blacktype a)
    |   IncHeight ((BlackNonLeaf blacktype) a) deriving (Show, Eq)

data PossibleHeightDecResult childblacktype a =
        NoDec ((BlackNonLeaf childblacktype) a)
    |   DecHeight (childblacktype a) deriving (Show, Eq)

data RemoveMinBlackResult childblacktype a =
        GoodBlack ((BlackNonLeaf childblacktype) a)
    |   BadBlack (childblacktype a)

-- Classes and Instances

class BlackNode b where
    blackHeight :: b a -> Int
    insBlack :: (Ord a) => Bool -> a -> b a -> RedOrBlackNode b a
    memberBlack :: (Ord a) => a -> b a -> Bool
    removeMinBlack :: (Ord a) => (BlackNonLeaf b) a -> (a, RemoveMinBlackResult b a)

-- | BlackLeaf is the simplest instance of BlackNode
--
-- >>> blackHeight (BlackLeaf :: BlackLeaf Int)
-- 0
--
-- >>> insBlack 234 BlackLeaf
-- Red (RedNode 234 BlackLeaf BlackLeaf)
--
-- >>> memberBlack 234 (insBlack 234 BlackLeaf)
-- True
--
-- >>> memberBlack 233 (insBlack 234 BlackLeaf)
-- False
--
instance BlackNode BlackLeaf where
    blackHeight _ = 0

    insBlack _ x BlackLeaf = Red $ RedNode x BlackLeaf BlackLeaf

    memberBlack _ _ = False

    -- If the min element is at a red node then we're good, remove that node and return the right child
    removeMinBlack (BlackNode y (Red (RedNode x BlackLeaf BlackLeaf)) right) = (x, GoodBlack (BlackNode y (Black BlackLeaf) right))
    -- If the min is at a black node with a red child, then we delete the node and paint the child black
    removeMinBlack (BlackNode x (Black BlackLeaf) (Red (RedNode y left right))) = (x, GoodBlack (BlackNode y (Black left) (Black right)))
    -- This is the complicated case, when the parent and child are both black (which must be a leaf).. this case must be bubbled
    -- up...
    removeMinBlack (BlackNode x (Black BlackLeaf) (Black BlackLeaf)) = (x, BadBlack BlackLeaf)

instance (BlackNode a) => BlackNode (BlackNonLeaf a) where
    blackHeight (BlackNode _ l _ ) = 1 + treeBlackHeight l

    insBlack dupes x n@(BlackNode y left right)
        | (x == y) && not dupes = Black n
        | x < y                 = lbalance y (insRedOrBlackNode dupes x left) right
        | otherwise             = rbalance y left (insRedOrBlackNode dupes x right)

    memberBlack x (BlackNode y left right)
        | x == y    = True
        | x < y     = memberRB x left
        | otherwise = memberRB x right

    removeMinBlack (BlackNode x (Black left) right) = (minElt, wrapResult $ balanceBlack x newLeft right)
        where
            wrapResult :: PossibleHeightDecResult b a-> RemoveMinBlackResult b a
            wrapResult (NoDec t)        = GoodBlack t
            wrapResult (DecHeight t)    = BadBlack t

            (minElt, newLeft)           = removeMinBlack left

    removeMinBlack (BlackNode x (Red (RedNode y sl sr)) right) = (minElt, GoodBlack $ BlackNode x (balanceRed y newSl sr) right)
        where
            (minElt, newSl) = removeMinBlack sl

-- Tree Functions

empty' :: RedOrBlackNode BlackLeaf a
empty' = Black BlackLeaf

insertRB :: (BlackNode b, Ord a) => Bool -> a -> b a -> PossibleHeightIncResult b a
insertRB dupes x = colourRootBlack . insBlack dupes x

memberRB :: (BlackNode b, Ord a) => a -> RedOrBlackNode b a -> Bool
memberRB x (Black blackNode) = memberBlack x blackNode
memberRB x (Red (RedNode y left right))
    | x == y    = True
    | x < y     = memberBlack x left
    | otherwise = memberBlack x right

-- Helper Functions

-- | returns the black height for a Red Node.
--
blackHeightRed :: (BlackNode b) => RedNode b a -> Int
blackHeightRed (RedNode _ l _) = blackHeight l

-- | returns the black height for a redblack tree
--
treeBlackHeight :: (BlackNode b) => RedOrBlackNode b a -> Int
treeBlackHeight (Black n)   = blackHeight n
treeBlackHeight (Red n)     = blackHeightRed n

-- | Returns a singleton tree, this isnt for external use but only for use
-- in inserting an element, it produces a tree witha red, when all externally
-- used instances should have black root nodes in their final state.
--
-- >>> singletonTree 1
-- Red (RedNode 1 BlackLeaf BlackLeaf)
--
singletonTree :: a -> RedOrBlackNode BlackLeaf a
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

insRedOrBlackNode :: (BlackNode b, Ord a) => Bool -> a -> RedOrBlackNode b a -> InsRedBlackResult b a
insRedOrBlackNode dupes x (Black blackNode)   = GoodTree $ insBlack dupes x blackNode
insRedOrBlackNode dupes x (Red redNode)       = case insRed dupes x redNode of
    RedLeft x' left right   -> RedBlackLeft x' left right
    RedRight x' left right  -> RedBlackRight x' left right
    Good goodRed            -> GoodTree $ Red goodRed

-- | during `insBlack` ops we may need to rebalance the tree,
-- breakages of invariant 1 bubble up the tree until they get to
-- a black node and either get rebalanced here or in `rbalance`
-- it gets called when trying to form a black node, and takes
-- the element at the intended black node, the possibly broken left child
-- and the good right child.
--
-- Since the intention is to create a black node, the depth of the tree will increase
-- by one.  This is true even if ultimately the result is a red node, as 2 Black nodes
-- are still added.
--
lbalance :: a -> InsRedBlackResult b a -> RedOrBlackNode b a -> RedOrBlackNode (BlackNonLeaf b) a
lbalance x (GoodTree left) right                    = Black $ BlackNode x left right
lbalance z (RedBlackLeft y (RedNode x a b) c) d     = Red $ RedNode y (BlackNode x (Black a) (Black b)) (BlackNode z (Black c) d)
lbalance z (RedBlackRight x a (RedNode y b c)) d    = Red $ RedNode y (BlackNode x (Black a) (Black b)) (BlackNode z (Black c) d)

-- | like `lbalance` but for use when you want to create a node with a
-- good left child tree and a broken right child tree
--
rbalance :: a -> RedOrBlackNode b a -> InsRedBlackResult b a -> RedOrBlackNode (BlackNonLeaf b) a
rbalance x left (GoodTree right)                    = Black $ BlackNode x left right
rbalance x a (RedBlackLeft z (RedNode y b c) d)     = Red $ RedNode y (BlackNode x a (Black b)) (BlackNode z (Black c) (Black d))
rbalance x a (RedBlackRight y b (RedNode z c d))    = Red $ RedNode y (BlackNode x a (Black b)) (BlackNode z (Black c) (Black d))

-- | In some cases when deleting elements from a Red Black Tree, the black height invariant (#2)
-- can get broken, this function attempts the rebalance it, for use when you are at a black node and have just run removeMin
-- on the left child tree.
--
balanceBlack :: (BlackNode b) =>  a -> RemoveMinBlackResult b a-> RedOrBlackNode (BlackNonLeaf b) a-> PossibleHeightDecResult (BlackNonLeaf b) a
balanceBlack x (GoodBlack left) right = NoDec $ BlackNode x (Black left) right
balanceBlack x n (Red (RedNode y sl sr)) = NoDec $ BlackNode y (balanceRed x n sl) (Black sr)
balanceBlack x (BadBlack n) (Black (BlackNode y (Black sl) (Black sr))) = DecHeight $ BlackNode x (Black n) (Red $ RedNode y sl sr)
balanceBlack x n (Black (BlackNode y (Red (RedNode z sll slr)) (Black sr))) = balanceBlack x n $ Black $ BlackNode z (Black sll) (Red $ RedNode y slr sr)
balanceBlack x (BadBlack n) (Black (BlackNode y sl (Red (RedNode z srl srr)))) = NoDec $ BlackNode y (Black $ BlackNode x (Black n) sl) (Black $ BlackNode z (Black srl) (Black srr))

-- | In some cases when deleting elements from a Red Black Tree, the black height invariant (#2)
-- can get broken, this function attempts the rebalance it for use when you are at a red node and have just run removeMin on the
-- left child tree
--
balanceRed :: (BlackNode b) =>  a -> RemoveMinBlackResult b a-> (BlackNonLeaf b) a-> RedOrBlackNode (BlackNonLeaf b) a
balanceRed x (GoodBlack left) right = Red $ RedNode x left right
balanceRed x (BadBlack n) (BlackNode y (Black sl) (Black sr)) = Black $ BlackNode x (Black n) $ Red $ RedNode y sl sr
balanceRed x n (BlackNode y (Red (RedNode z sll slr)) (Black sr)) = balanceRed x n $ BlackNode z (Black sll) (Red $ RedNode y slr sr)
balanceRed x (BadBlack n) (BlackNode y sl (Red (RedNode z srl srr))) = Red $ RedNode y (BlackNode x (Black n) sl) (BlackNode z (Black srl) (Black srr))

-- | Colours the root Black if necessary, potentially increasing the black height of the tree.
--
colourRootBlack :: RedOrBlackNode b a -> PossibleHeightIncResult b a
colourRootBlack (Black t)                       = SameHeight t
colourRootBlack (Red (RedNode x left right))    = IncHeight $ (BlackNode x `on` Black) left right
