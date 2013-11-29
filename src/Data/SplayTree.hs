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

module Data.SplayTree where

import Data.Maybe ( fromMaybe )
import Control.Comonad

-- | From Section 5.4 of PFDS:
-- "Splay Trees are perhaps the most famous and successful of all amortized
-- data structures.  Splay trees are a close relative of balanced binary
-- search trees but they maintain no explicit balance information.  Instead
-- every operation blindly restructures the tree using some simple
-- transformations that tend to increase balance."
--
-- Although every operation can be as bad as O(n), every operation runs in
-- O(log n) amortized time.

-- | The Splay tree data type resembles the Binary search tree data type
--
data SplayTree a =
        Leaf
    |   Node a (SplayTree a) (SplayTree a) deriving (Show, Eq)

data Direction = SLeft | SRight deriving (Show, Eq)

data SplayTree' a =
        Left' a (SplayTree a)
    |   Right' a (SplayTree a) deriving (Show, Eq)

data SplayTreeZipper a = SplayTreeZipper
    {   getCurrentFocus :: a
    ,   getLeftTree     :: SplayTree a
    ,   getRightTree    :: SplayTree a
    ,   getCrumbs       :: [SplayTree' a]
    } deriving (Show, Eq)

-- instances

instance Functor SplayTree where
    fmap _ Leaf         = Leaf
    fmap f (Node x l r) = Node (f x) (f `fmap` l) (f `fmap` r)

instance Functor SplayTree' where
    fmap f (Left' x c) = Left' (f x) (f `fmap` c)
    fmap f (Right' x c) = Right' (f x) (f `fmap` c)

instance Functor SplayTreeZipper where
    fmap f (SplayTreeZipper focus left right crumbs) = SplayTreeZipper (f focus) (f `fmap` left) (f `fmap` right) (fmap (fmap f) crumbs)

-- | Requires the following properties:
-- >    extend extract      = id
-- >    extract . extend f  = id
-- >    extend f . extend g = extend (f . extend g)
instance Comonad SplayTreeZipper where
    extract (SplayTreeZipper x _ _ _) = x

    duplicate z@(SplayTreeZipper _ l r _) = case goUp z of
        Nothing -> SplayTreeZipper z (zipzipTree l) (zipzipTree r) []
        Just z' -> fromMaybe (SplayTreeZipper z (zipzipTree l) (zipzipTree r) []) $ repeatLastMove z (duplicate z')
            where
                repeatLastMove :: SplayTreeZipper a -> SplayTreeZipper b -> Maybe (SplayTreeZipper b)
                repeatLastMove (SplayTreeZipper _ _ _ [])               = const Nothing
                repeatLastMove (SplayTreeZipper _ _ _ (Left' _ _:_))    = goLeft
                repeatLastMove (SplayTreeZipper _ _ _ (Right' _ _:_))   = goRight

-- Zipper functions

(<.) :: SplayTree' a -> SplayTree a -> SplayTree a
(Left' x r) <. t   = Node x t r
(Right' x l) <. t  = Node x l t

(<+) :: [SplayTree' a] -> SplayTree a -> SplayTree a
ss <+ t = foldr (<.) t ss

unzip :: SplayTreeZipper a -> SplayTree a
unzip (SplayTreeZipper x l r ss) = ss <+ Node x l r

zipTree :: SplayTree a -> Maybe (SplayTreeZipper a)
zipTree Leaf            = Nothing
zipTree (Node x l r)    = Just $ SplayTreeZipper x l r []

zipzipTree :: SplayTree a -> SplayTree (SplayTreeZipper a)
zipzipTree t = case zipTree t of
    Nothing                             -> Leaf
    Just z@(SplayTreeZipper _ l r _)    -> Node z (zipzipTree l) (zipzipTree r)

getFocusNode :: SplayTreeZipper a -> SplayTree a
getFocusNode (SplayTreeZipper x l r _) = Node x l r

goLeft :: SplayTreeZipper a -> Maybe (SplayTreeZipper a)
goLeft (SplayTreeZipper _ Leaf _ _)             = Nothing
goLeft (SplayTreeZipper x (Node y ll lr) r ss)  = Just $ SplayTreeZipper y ll lr (Left' x r : ss)

goRight :: SplayTreeZipper a -> Maybe (SplayTreeZipper a)
goRight (SplayTreeZipper _ _ Leaf _)             = Nothing
goRight (SplayTreeZipper x l (Node y rl rr) ss)  = Just $ SplayTreeZipper y rl rr (Right' x l : ss)

goUp :: SplayTreeZipper a -> Maybe (SplayTreeZipper a)
goUp (SplayTreeZipper _ _ _ [])                 = Nothing
goUp (SplayTreeZipper x ll lr (Left' y r:ss))   = Just $ SplayTreeZipper y (Node x ll lr) r ss
goUp (SplayTreeZipper x rl rr (Right' y l:ss))  = Just $ SplayTreeZipper y l (Node x rl rr) ss
