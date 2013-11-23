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

module Data.Heap.ExplicitMin where

import Data.Heap

import Control.Applicative

-- | Exercise 3.7:
-- Implement a functor that takes in any implementation of a Heap
-- and returns a Heap such that findMin takes only O(1) time
-- (like a leftist heap) rather than O(log n) time
-- (like a binomial heap),
-- and insert, merge and deleteMin take O(log n) time (assuming all four
-- take O(log n) time or better in the original implementation)

data ExplicitMin h a =
        EmptyExplicitMin
    |   ExplicitMin a (h a) deriving (Show, Eq)

instance (Heap h) => Heap (ExplicitMin h) where
    singletonHeap = ExplicitMin <*> singletonHeap

    empty = EmptyExplicitMin

    isEmpty EmptyExplicitMin    = True
    isEmpty _                   = False

    merge x EmptyExplicitMin                    = x
    merge EmptyExplicitMin x                    = x
    merge (ExplicitMin x h) (ExplicitMin x' h') = ExplicitMin (min x x') (h `merge` h')

    findMin EmptyExplicitMin    = Nothing
    findMin (ExplicitMin x _)   = Just x

    deleteMin EmptyExplicitMin  = EmptyExplicitMin
    deleteMin (ExplicitMin _ h) = case findMin h' of
        Nothing -> EmptyExplicitMin
        Just x  -> ExplicitMin x h'
        where
            h' = deleteMin h

