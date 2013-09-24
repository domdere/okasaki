module Data.Heap.Leftist.HeightBiased
    (   HBLHeap
    ,   leftistProperty
    ,   rank
    ,   singletonHeap
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
-- O(1)
--
-- prop> (\x a b -> leftistProperty (makeTree x a b)) (x :: Int) (a :: HBLHeap Int) (b :: HBLHeap Int)
--
-- >>> makeTree 3 EmptyHeap (makeTree 1 EmptyHeap EmptyHeap)
-- Tree 1 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap
--
-- >>> makeTree 3 (makeTree 1 EmptyHeap EmptyHeap) EmptyHeap
-- Tree 1 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap
--
makeTree :: a -> HBLHeap a -> HBLHeap a -> HBLHeap a
makeTree x (HBLHeap a) (HBLHeap b)
    | leftistRank a < leftistRank b = fromLeftist $ Tree (leftistRank a + 1) x b a
    | otherwise                     = fromLeftist $ Tree (leftistRank b + 1) x a b

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

-- | Exercise 3.2: define insert directly rather than through a call
-- to merge.
--
addTo :: (Ord a) => a -> HBLHeap a -> HBLHeap a
x `addTo` h = 
    case toLeftist h of
        EmptyHeap       -> singletonHeap x
        (Tree _ y a b)  ->
            if x > y then
                combineTrees y (fromLeftist a) (x `addTo` fromLeftist b)
            else
                combineTrees x (fromLeftist EmptyHeap) h

-- | Exercise 3.4
-- A **weight biased** leftist heap is one where the rank is instead defined by the number of elements contained in the tree.
--weightBiasedRank :: HBLHeap a -> Int
--weightBiasedRank EmptyHeap              = 0
--weightBiasedRank (Tree _ _ left right)  = weightBiasedRank left + weightBiasedRank right

-- | weight biased leftist heaps have the following revised leftist property, true for all weight biased leftist heaps
--weightBiasedLeftistProperty :: HBLHeap a -> Bool
--weightBiasedLeftistProperty EmptyHeap               = True
--weightBiasedLeftistProperty (Tree _ _ left right)   = weightBiasedRank left >= weightBiasedRank right

-- | Exercise 3.4 (a) Prove that in a weight biased leftist heap, the right spine contains at most floor (log (n + 1)) elements,
-- or if r is the number of elements in the right spine, then
--      r <= floor(log (n + 1))
--      r <= log(n + 1) (r <- Z, and there are no more integers in [floor(log (n + 1)), log(n + 1)])
--      2^r <= n + 1
--      n => 2^r - 1
-- (WIP)
