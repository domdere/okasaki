{-
Copyright (c) 2013 Dom De Re

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

module Data.SplayHeap where

import Control.Comonad
import Control.Monad ( liftM2 )
import Data.Function ( on )
import Data.Maybe ( fromMaybe )

-- | From Section 5.4 of PFDS:
-- "Splay Trees are perhaps the most famous and successful of all amortized
-- data structures.  Splay trees are a close relative of balanced binary
-- search trees but they maintain no explicit balance information.  Instead
-- every operation blindly restructures the tree using some simple
-- transformations that tend to increase balance."
--
-- Although every operation can be as bad as O(n), every operation runs in
-- O(log n) amortized time.

-- $setup
--
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
--
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (SplayHeap a) where arbitrary = fromListL <$> arbitrary

-- | The Splay tree data type resembles the Binary search tree data type
--
data SplayHeap a =
        Leaf
    |   Node a (SplayHeap a) (SplayHeap a) deriving (Show, Eq)

data NonEmptySplayHeap a = NonEmptySplayHeap a (SplayHeap a) (SplayHeap a) deriving (Show, Eq)

data Direction = SLeft | SRight deriving (Show, Eq)

data SplayHeap' a =
        Left' a (SplayHeap a)
    |   Right' a (SplayHeap a) deriving (Show, Eq)

data SplayHeapZipper a = SplayHeapZipper
    {   getCurrentFocus :: a
    ,   getLeftTree     :: SplayHeap a
    ,   getRightTree    :: SplayHeap a
    ,   getCrumbs       :: [SplayHeap' a]
    } deriving (Show, Eq)

-- instances

instance Functor SplayHeap where
    fmap _ Leaf         = Leaf
    fmap f (Node x l r) = Node (f x) (f `fmap` l) (f `fmap` r)

instance Functor NonEmptySplayHeap where
    fmap f (NonEmptySplayHeap x l r) = (NonEmptySplayHeap (f x) `on` fmap f) l r

instance Functor SplayHeap' where
    fmap f (Left' x c) = Left' (f x) (f `fmap` c)
    fmap f (Right' x c) = Right' (f x) (f `fmap` c)

instance Functor SplayHeapZipper where
    fmap f (SplayHeapZipper focus left right crumbs) = SplayHeapZipper (f focus) (f `fmap` left) (f `fmap` right) (fmap (fmap f) crumbs)

-- | Requires the following properties:
-- >    extend extract      = id
-- >    extract . extend f  = id
-- >    extend f . extend g = extend (f . extend g)
instance Comonad SplayHeapZipper where
    extract (SplayHeapZipper x _ _ _) = x

    duplicate z@(SplayHeapZipper _ l r _) = case goUp z of
        Nothing -> SplayHeapZipper z (zipzipTree l) (zipzipTree r) []
        Just z' -> fromMaybe (SplayHeapZipper z (zipzipTree l) (zipzipTree r) []) $ repeatLastMove z (duplicate z')
            where
                repeatLastMove :: SplayHeapZipper a -> SplayHeapZipper b -> Maybe (SplayHeapZipper b)
                repeatLastMove (SplayHeapZipper _ _ _ [])               = const Nothing
                repeatLastMove (SplayHeapZipper _ _ _ (Left' _ _:_))    = goLeft
                repeatLastMove (SplayHeapZipper _ _ _ (Right' _ _:_))   = goRight

-- Zipper functions

-- | returns True if the crumb is a left crumb
--
-- >>> isLeft (Left' 1 Leaf)
-- True
--
-- >>> isLeft (Right' 1 Leaf)
-- False
--
isLeft :: SplayHeap' a -> Bool
isLeft (Left' {})   = True
isLeft (Right' {})  = False

-- | returns True if the crumb is a right crumb
--
-- >>> isRight (Left' 1 Leaf)
-- False
--
-- >>> isRight (Right' 1 Leaf)
-- True
--
isRight :: SplayHeap' a -> Bool
isRight  = not . isLeft

-- | Plugs a Heap into a 1-Hole Context to form a larger Heap
--
-- >>> (Right' 3 Leaf) <. (Node 6 (Node 4 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf)))
-- Node 3 Leaf (Node 6 (Node 4 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf)))
--
-- >>> (Left' 6 (Node 7 Leaf (Node 8 Leaf Leaf))) <. (Node 4 Leaf Leaf)
-- Node 6 (Node 4 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf))
--
(<.) :: SplayHeap' a -> SplayHeap a -> SplayHeap a
(Left' x r) <. t   = Node x t r
(Right' x l) <. t  = Node x l t

(<+) :: [SplayHeap' a] -> SplayHeap a -> SplayHeap a
ss <+ t = foldr (<.) t ss

unzip :: SplayHeapZipper a -> SplayHeap a
unzip = liftM2 (<+) getCrumbs getFocusNode

zipTree :: SplayHeap a -> Maybe (SplayHeapZipper a)
zipTree = performNonEmptyOp zipNonEmptyTree

zipNonEmptyTree :: NonEmptySplayHeap a -> SplayHeapZipper a
zipNonEmptyTree (NonEmptySplayHeap x l r) = SplayHeapZipper x l r []

zipzipTree :: SplayHeap a -> SplayHeap (SplayHeapZipper a)
zipzipTree t = case zipTree t of
    Nothing                             -> Leaf
    Just z@(SplayHeapZipper _ l r _)    -> Node z (zipzipTree l) (zipzipTree r)

getFocusNode :: SplayHeapZipper a -> SplayHeap a
getFocusNode (SplayHeapZipper x l r _) = Node x l r

goLeft :: SplayHeapZipper a -> Maybe (SplayHeapZipper a)
goLeft (SplayHeapZipper _ Leaf _ _)             = Nothing
goLeft (SplayHeapZipper x (Node y ll lr) r ss)  = Just $ SplayHeapZipper y ll lr (Left' x r : ss)

goRight :: SplayHeapZipper a -> Maybe (SplayHeapZipper a)
goRight (SplayHeapZipper _ _ Leaf _)             = Nothing
goRight (SplayHeapZipper x l (Node y rl rr) ss)  = Just $ SplayHeapZipper y rl rr (Right' x l : ss)

goUp :: SplayHeapZipper a -> Maybe (SplayHeapZipper a)
goUp (SplayHeapZipper _ _ _ [])                 = Nothing
goUp (SplayHeapZipper x ll lr (Left' y r:ss))   = Just $ SplayHeapZipper y (Node x ll lr) r ss
goUp (SplayHeapZipper x rl rr (Right' y l:ss))  = Just $ SplayHeapZipper y l (Node x rl rr) ss

searchTreeZipper :: (Ord a) => a -> SplayHeapZipper a -> Maybe (SplayHeapZipper a)
searchTreeZipper x z
    | x == extract z    = Just z
    | x < extract z     = goLeft z >>= searchTreeZipper x
    | otherwise         = goRight z >>= searchTreeZipper x

rotate :: SplayHeapZipper a -> SplayHeapZipper a
rotate z@(SplayHeapZipper _ _ _ [])                 = z
rotate (SplayHeapZipper y ll lr (s@(Left' {}):ss))  = SplayHeapZipper y ll (s <. lr) ss
rotate (SplayHeapZipper y rl rr (s@(Right' {}):ss)) = SplayHeapZipper y (s <. rl) rr ss

findMin' :: SplayHeapZipper a -> a
findMin' z = case goLeft z of
    Nothing -> extract z
    Just z' -> findMin' z'

deleteMin' :: SplayHeapZipper a -> Maybe (SplayHeapZipper a)
deleteMin' z = case (goLeft z, getCrumbs z, getRightTree z) of
    (Nothing, [], Leaf)             -> Nothing
    (Nothing, Left' x r:ss, Leaf)   -> Just $ SplayHeapZipper x Leaf r ss
    (Nothing, Right' x l:ss, Leaf)  -> Just $ SplayHeapZipper x l Leaf ss
    (Nothing, ss, Node x r l)       -> Just $ SplayHeapZipper x r l ss
    (Just z', _, _)                 -> deleteMin' $ rotate z'

-- SplayHeap Methods

-- | Converts a non empty SplayHeap into a plain splay heap type
toHeap :: NonEmptySplayHeap a -> SplayHeap a
toHeap (NonEmptySplayHeap x l r) = Node x l r

toNonEmptySplayTree :: SplayHeap a -> Maybe (NonEmptySplayHeap a)
toNonEmptySplayTree Leaf = Nothing
toNonEmptySplayTree (Node x l r) = Just $ NonEmptySplayHeap x l r

-- | runs an operation on a non empty splay heap on a general splay heap,
-- returning `Nothing` when the splay heap is empty
--
performNonEmptyOp :: (NonEmptySplayHeap a -> b) -> SplayHeap a -> Maybe b
performNonEmptyOp f t = do
    net <- toNonEmptySplayTree t
    return $ f net

bigger :: (Ord a) => a -> SplayHeap a -> SplayHeap a
bigger _ Leaf = Leaf
bigger pivot t@(Node x l r)
    | x <= pivot = bigger pivot r
    | otherwise = case l of
        Leaf            -> t
        Node y ll lr    -> if y <= pivot
            then
                Node x (bigger pivot lr) r
            else
                Node y (bigger pivot ll) (Node x lr r)

-- | Exercise 5.4: goes through a heap and returns the
-- heap containing elements smaller than the pivot provided,
-- with rotations happening along the way
--
-- >>> smaller 6 $ Node 4 Leaf (Node 8 Leaf Leaf)
-- Node 4 Leaf Leaf
--
smaller :: (Ord a) => a -> SplayHeap a -> SplayHeap a
smaller _ Leaf = Leaf
smaller pivot t@(Node x l r)
    | x > pivot = smaller pivot l
    | otherwise = case r of
        Leaf            -> t
        Node y rl rr    -> if y > pivot
            then
                Node x l (smaller pivot rl)
            else
                Node y (Node x rl l) (smaller pivot rr)

-- | adds an element to the tree.
--
-- >>> 6 `addedTo` fromListR [4,8]
-- Node 6 (Node 4 Leaf Leaf) (Node 8 Leaf Leaf)
--
addedTo :: (Ord a) => a -> SplayHeap a -> SplayHeap a
x `addedTo` t = Node x (smaller x t) (bigger x t)

findMin :: SplayHeap a -> Maybe a
findMin t = case zipTree t of
    Nothing -> Nothing
    Just z  -> Just $ findMin' z

-- | creates a SplayHeap from a list by inserting elements into an empty tree from left to right
--
fromListL :: (Ord a) => [a] -> SplayHeap a
fromListL = foldl (flip addedTo) Leaf

-- | creates a SplayHeap from a list by inserting elements into an empty tree from right to left
--
fromListR :: (Ord a) => [a] -> SplayHeap a
fromListR = foldr addedTo Leaf

