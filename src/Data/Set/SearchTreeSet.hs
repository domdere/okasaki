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

module Data.Set.SearchTreeSet where

import Data.SearchTree
import Data.Set

-- | A wrapper that gives a SearchTree a Set instance
newtype SearchTreeSet t a = SearchTreeSet
    {   fromSearchTreeSet :: t a
    } deriving (Show, Eq)

instance (SearchTree t) => Set (SearchTreeSet t) where
    empty = SearchTreeSet emptyTree
    insert x s = SearchTreeSet $ treeInsertIfMissing x $ fromSearchTreeSet s
    remove x s = SearchTreeSet $ removedFromTree x $ fromSearchTreeSet s
    member x s = treeMember x $ fromSearchTreeSet s
