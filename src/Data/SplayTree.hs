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

data Direction = Left | Right deriving (Show, Eq)

data SplayTreeZipperCrumb a = SplayTreeZipperCrumb
    {   getDirection    :: Direction
    ,   nodeElement     :: a
    ,   otherChild      :: SplayTree a
    } deriving (Show, Eq)

data SplayTreeZipper a = SplayTreeZipper
    {   getCurrentNode  :: SplayTree a
    ,   getCrumbs       :: [SplayTreeZipperCrumb a]
    } deriving (Show, Eq)

-- instances

instance Functor SplayTree where
    fmap _ Leaf         = Leaf
    fmap f (Node x l r) = Node (f x) (f `fmap` l) (f `fmap` r)

instance Functor SplayTreeZipperCrumb where
    fmap f (SplayTreeZipperCrumb d x c) = SplayTreeZipperCrumb d (f x) (f `fmap` c)

instance Functor SplayTreeZipper where
    fmap f (SplayTreeZipper focus crumbs) = SplayTreeZipper (f `fmap` focus) (fmap (fmap f) crumbs)
