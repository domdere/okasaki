module Data.Heap.Binomial.Heap 
    (
    ) where

import Data.Heap
import Data.Heap.Binomial.Tree

import Data.Function
import Data.Maybe

-- | From Section 3.2 of Okasaki's *Purely Functional Data Structures* book,
--
-- "A binomial heap is a collection of heap ordered binomial trees in which no two trees have the same rank"
--
newtype BinomialHeap a = BinomialHeap [BinomialTree a] deriving (Show, Eq)

-- Invariants

-- | the trees are in order of increasing rank
--
treesInOrderOfIncreasingRank :: BinomialHeap a -> Bool
treesInOrderOfIncreasingRank (BinomialHeap [])          = True
treesInOrderOfIncreasingRank (BinomialHeap (_:[]))      = True
treesInOrderOfIncreasingRank (BinomialHeap (x:y:xs))    =
    ((<=) `on` treeRank) x y && treesInOrderOfIncreasingRank (BinomialHeap $ y:xs)

-- Heap instance

-- | creates a singletonHeap
--
-- >>> singletonHeap' 4
-- BinomialHeap [BinomialTree 0 4 []]
--
singletonHeap' :: a -> BinomialHeap a
singletonHeap' x = BinomialHeap [singletonTree x]

-- | creates the empty Heap instance
--
-- >>> empty' :: BinomialHeap Int
-- BinomialHeap []
--
empty' :: BinomialHeap a
empty' = BinomialHeap []

-- | checks to see if there any elements in the heap
--
isEmpty' :: BinomialHeap a -> Bool
isEmpty' (BinomialHeap [])  = True
isEmpty' _                  = False

-- | Inserts a Tree into a heap
--
insTree :: (Ord a) => BinomialTree a -> BinomialHeap a -> BinomialHeap a
t `insTree` (BinomialHeap []) = BinomialHeap [t]
t' `insTree` (BinomialHeap ts@(t:ts'))
    | t' < t = BinomialHeap (t':ts)
    | otherwise = t' `link` t `insTree` BinomialHeap ts'

-- | Merges two Binomial Heaps together
--
merge' :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
(BinomialHeap []) `merge'` a = a
a `merge'` (BinomialHeap []) = a
w@(BinomialHeap (a:as)) `merge'` z@(BinomialHeap (b:bs))
    | a < b = BinomialHeap $ a:asMergez
    | a > b = BinomialHeap $ b:wMergebs
    | otherwise = (a `link` b) `insTree` asMergebs
    where
        BinomialHeap asMergez   = BinomialHeap as `merge'` z
        BinomialHeap wMergebs   = w `merge'` BinomialHeap bs
        asMergebs               = BinomialHeap as `merge'` BinomialHeap bs

-- | finds the minimum element of the heap
--
-- >>> findMin' (empty' :: BinomialHeap Int)
-- Nothing
--
-- >>> findMin' (singtonHeap' 4)
-- Just 4
--
findMin' :: BinomialHeap a -> Maybe a
findMin' h = (\(BinomialTree _ x _, _) -> x) `fmap` removeMinTree h

-- | Deletes the minimum element
--
deleteMin' :: (Ord a) => BinomialHeap a -> BinomialHeap a
deleteMin' h = maybe empty' mergeChildren $ removeMinTree h
    where
        mergeChildren (BinomialTree _ _ ts, h') =  BinomialHeap (reverse ts) `merge'` h'

instance Heap BinomialHeap where
    singletonHeap   = singletonHeap'
    empty           = empty'
    isEmpty         = isEmpty'

    merge = merge'

    findMin     = findMin'
    deleteMin   = deleteMin'

-- Helper Functions

-- | Returns the tree containing the minimum element and
-- the heap with that tree removed
removeMinTree :: BinomialHeap a -> Maybe (BinomialTree a, BinomialHeap a)
removeMinTree (BinomialHeap [])     = Nothing
removeMinTree (BinomialHeap (t:ts)) = Just (t, BinomialHeap ts)
