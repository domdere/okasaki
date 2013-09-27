module Data.Heap.Binomial.Heap 
    (
    ) where

import Data.Heap.Binomial.Tree

-- | From Section 3.2 of Okasaki's *Purely Functional Data Structures* book,
--
-- "A binomial heap is a collection of heap ordered binomial trees in which no two trees have the same rank"
--
newtype BinomialHeap a = BinomialHeap [BinomialTree a] deriving (Show, Eq)

-- Invariants

-- | the trees are in order of increasing rank
--
treesInOrderOfIncreasingRank :: BinomialHeap a -> Bool
treesInOrderOfIncreasingRank (BinomialHeap []) = True
treesInOrderOfIncreasingRank (BinomialHeap (x:[])) = True
treesInOrderOfIncreasingRank (BinomialHeap (x:y:xs)) = ((<=) `on` treeRank) x y && treesInOrderOfIncreasingRank (BinomialHeap $ y:xs)

-- Heap instance

-- | creates a singletonHeap
--
singletonHeap' :: a -> BinomialHeap a



-- class Heap h where
--     singletonHeap :: a -> h a
--     empty :: h a
--     isEmpty :: h a -> Bool
--
--     merge :: Ord a => h a -> h a -> h a
--
--     findMin :: Ord a => h a -> Maybe a
--     deleteMin :: Ord a => h a -> h a

