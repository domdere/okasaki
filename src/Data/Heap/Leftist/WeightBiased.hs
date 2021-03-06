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

module Data.Heap.Leftist.WeightBiased
    (   WBLHeap
    ) where

import Control.Applicative hiding (empty)
import Data.Function
import Data.Maybe

import Data.Heap
import Data.Heap.Leftist

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (WBLHeap a) where arbitrary = fromList <$> arbitrary

-- | from Chapter 3 of "Purely Functional Data Structures" by Chris Okasaki
-- "Leftist Heaps are heap ordered binary trees that satisfy the leftist property:
-- the rank of any left child is at least as large as the rank of its right sibling"
--
-- Exercise 3.4
-- A **weight biased** leftist heap is one where the rank is instead defined by the number of elements contained in the tree.
--
newtype WBLHeap a = WBLHeap (LeftistHeap a) deriving (Show, Eq)

-- | a helper function Okasaki uses as part of the merge function
-- for the WBLHeap.
-- It takes the element, and two leftist heaps,
-- and constructs the node which contains the element
-- and the two given heaps as children, such that the leftist
-- property is satisfied.
--
-- The difference between the Weight biased one and the 'Normal' Height biased one
-- is that the fused tree has a rank set to the sum of the left and right trees,
-- instead of "the rank of the right child tree, + 1"
--
-- runs in O(1) time
--
-- prop> (\x a b -> leftistProperty (makeTree x a b)) (x :: Int) (a :: WBLHeap Int) (b :: WBLHeap Int)
--
-- >>> makeTree 3 (fromLeftist EmptyHeap) (((makeTree 1) `on` fromLeftist) EmptyHeap EmptyHeap)
-- WBLHeap (Tree 2 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap)
--
-- >>> makeTree 3 (((makeTree 1) `on` fromLeftist) EmptyHeap EmptyHeap) (fromLeftist EmptyHeap)
-- WBLHeap (Tree 2 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap)
--
makeTree :: Eq a => a -> WBLHeap a -> WBLHeap a -> WBLHeap a
makeTree x (WBLHeap a) (WBLHeap b)
    | a < b     = fromLeftist $ Tree ((a `pluus` b) + 1) x b a
    | otherwise = fromLeftist $ Tree ((a `pluus` b) + 1) x a b
        where
            pluus = (+) `on` leftistRank

instance Leftist WBLHeap where
    rank = leftistRank . toLeftist

    toLeftist (WBLHeap x) = x
    fromLeftist = WBLHeap

    combineTrees = makeTree

instance Heap WBLHeap where
    singletonHeap = fromLeftist  . singletonLeftistHeap
    empty = fromLeftist EmptyHeap
    isEmpty = isEmptyLeftist . toLeftist
    merge = mergeLeftist

    findMin = findLeftistMin . toLeftist
    deleteMin = deleteLeftistMin

-- | Exercise 3.4
-- (a) Prove that in a weight biased leftist heap, the right spine contains at most floor (log (n + 1)) elements,
-- or if r is the number of elements in the right spine, then
--      r <= floor(log (n + 1))
--      r <= log(n + 1) (r <- Z, and there are no more integers in [floor(log (n + 1)), log(n + 1)])
--      2^r <= n + 1
--      n => 2^r - 1
-- (WIP)
