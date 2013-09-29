module Data.Heap.ExplicitMin where

import Data.Heap

import Control.Monad

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
    singletonHeap = ExplicitMin `ap` singletonHeap

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

