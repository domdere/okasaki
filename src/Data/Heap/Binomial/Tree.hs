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

module Data.Heap.Binomial.Tree
    (   BinomialTree(..)
    ,   link
    ,   treeRank
    ,   singletonTree
    ,   decreasingRank
    ,   eltsInHeapOrder
    ,   isStrictlyDecreasing
    ) where

import Data.Function

-- | From Chapter 3 of Purely Functional Data Structures,
-- Binomial Trees are defined inductively as:
--      *   A singleton node has rank 0
--      *   A binomial tree of rank r + 1 is formed by linking 2
--      *   binomial trees of rank r, having one tree as the
--      *   child of the other
--
-- A binomial tree or rank r can also be defined as a node
-- containing an element and r children trees t1, t2,..,ti,..,tr
-- where each ti has rank r - i. Hence the definition of BinomialTree,
-- Note that the children are to be stored in order of decreasing rank and
-- elements are to be stored in heap order.
--
data BinomialTree a = BinomialTree Int a [BinomialTree a] deriving (Show, Eq)

-- | the tree rank is defined as above, hence it can be calculated
-- from the length of the list of children.
-- runs in O(r) time
treeRank :: BinomialTree a -> Int
treeRank (BinomialTree rank _ _) = rank

-- | A singleton node/tree
--
-- All singleton trees should have rank 0.
-- prop> (\x -> treeRank (singletonTree x) == 0) (x :: Int)
--
-- this is the base case for our induction,
-- provided out singletons satify this property,
-- and our operations maintain it, it should remain true
-- (and the data constructor for the binomial tree is encapsulated
-- in this module).  Technically this property is trivially
-- satisfied.
-- prop> (\x -> decreasingRank (singletonTree x)) (x :: Int)
--
singletonTree :: a -> BinomialTree a
singletonTree x = BinomialTree 0 x []

-- | links two trees with the elements in heap order
-- this operation will work when the ranks of the
-- operands are not the same however (as the operation still
-- "makes sense"), in creating Binomial Heaps with this operation, 
-- only trees of the same rank get linked.
--
-- >>> link (singletonTree 3) (singletonTree 4)
-- BinomialTree 1 3 [BinomialTree 0 4 []]
--
-- >>> link (singletonTree 5) (singletonTree 2)
-- BinomialTree 1 2 [BinomialTree 0 5 []]
--
link :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
link w@(BinomialTree rw x ws) z@(BinomialTree rz y zs)
    | x < y                     = BinomialTree (rw + 1) x (z:ws)
    | otherwise                 = BinomialTree (rz + 1) y (w:zs)

-- Invariants

-- | The children of the binomial trees are expected to be in order
-- decreasing rank, hence this property becomes an invariant...
--
decreasingRank :: BinomialTree a -> Bool
decreasingRank (BinomialTree _ _ children) =
    isStrictlyDecreasing $ treeRank `fmap` children

-- | elements are to be stored in heap order..
--
eltsInHeapOrder :: (Ord a) => BinomialTree a -> Bool
eltsInHeapOrder t = isMonotonicallyIncreasing $ toList' t

-- Instances

instance (Eq a) => Ord (BinomialTree a) where
    compare = compare `on` treeRank

-- Helpers

-- | Just checks to see if a list is strictly decreasing.
--
-- >>> isStrictlyDecreasing [1,2,5,7,9]
-- False
--
-- >>> isStrictlyDecreasing [9,7,5,4,2,2,1]
-- False
--
-- >>> isStrictlyDecreasing [9,7,4,5,2,2,1]
-- False
--
-- >>> isStrictlyDecreasing [9,7,4,5,2,1]
-- False
--
isStrictlyDecreasing :: (Ord a) => [a] -> Bool
isStrictlyDecreasing []        = True
isStrictlyDecreasing (_:[])    = True
isStrictlyDecreasing (x:y:xs)  = (x > y) && isStrictlyDecreasing (y:xs)

-- | Just checks to see if a list is monotonically increasing.
--
-- >>> isMonotonicallyIncreasing [1,2,5,7,9]
-- True
--
-- >>> isMonotonicallyIncreasing [1,2,2,5,7,9]
-- True
--
-- >>> isMonotonicallyIncreasing [1,2,5,2,7,9]
-- False
--
-- >>> isMonotonicallyIncreasing [9,7,5,4,2,2,1]
-- False
--
isMonotonicallyIncreasing :: (Ord a) => [a] -> Bool
isMonotonicallyIncreasing []        = True
isMonotonicallyIncreasing (_:[])    = True
isMonotonicallyIncreasing (x:y:xs)  = (x <= y) && isMonotonicallyIncreasing (y:xs)


-- | pulls outs the elements of the binomial tree:
--
toList' :: BinomialTree a -> [a]
toList' (BinomialTree _ x []) = [x]
toList' (BinomialTree _ x ts) = x : foldr (\t xs -> toList' t ++ xs) [] ts
