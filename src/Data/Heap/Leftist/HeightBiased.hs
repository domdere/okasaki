module Data.Heap.Leftist.HeightBiased
    (   HBLHeap
    ) where

import Control.Applicative hiding (empty)
import Data.Function
import Data.Maybe

import Data.Heap
import Data.Heap.Leftist

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (HBLHeap a) where arbitrary = fromList <$> arbitrary

-- | from Chapter 3 of "Purely Functional Data Structures" by Chris Okasaki
-- "Leftist Heaps are heap ordered binary trees that satisfy the leftist property:
-- the rank of any left child is at least as large as the rank of its right sibling,
-- where the rank of a tree is defined to be the length of its right spine."
--
newtype HBLHeap a = HBLHeap (LeftistHeap a) deriving (Show, Eq)

-- | a helper function Okasaki uses as part of the merge function
-- for the HBLHeap.
-- It takes the element, and two leftist heaps,
-- and constructs the node which contains the element
-- and the two given heaps as children, such that the leftist
-- property is satisfied.
--
-- This uses
--
-- O(1)
--
-- prop> (\x a b -> leftistProperty (makeTree x a b)) (x :: Int) (a :: HBLHeap Int) (b :: HBLHeap Int)
--
-- >>> makeTree 3 (fromLeftist EmptyHeap) (((makeTree 1) `on` fromLeftist) EmptyHeap EmptyHeap)
-- HBLHeap (Tree 1 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap)
--
-- >>> makeTree 3 (((makeTree 1) `on` fromLeftist) EmptyHeap EmptyHeap) (fromLeftist EmptyHeap)
-- HBLHeap (Tree 1 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap)
--
makeTree :: Eq a => a -> HBLHeap a -> HBLHeap a -> HBLHeap a
makeTree x (HBLHeap a) (HBLHeap b)
    | a < b     = fromLeftist $ Tree (leftistRank a + 1) x b a
    | otherwise = fromLeftist $ Tree (leftistRank b + 1) x a b

instance Leftist HBLHeap where
    toLeftist (HBLHeap x) = x
    fromLeftist = HBLHeap

    combineTrees = makeTree

instance Heap HBLHeap where
    rank = leftistRank . toLeftist

    singletonHeap = fromLeftist  . singletonLeftistHeap
    empty = fromLeftist EmptyHeap
    isEmpty = isEmptyLeftist . toLeftist
    merge = mergeLeftist

    findMin = findLeftistMin . toLeftist
    deleteMin = deleteLeftistMin

-- Exercises

-- | Exercise 3.1: prove that the right spine of a leftist heap contains at most floor (log (n + 1))
-- elements.
--
-- TODO.
