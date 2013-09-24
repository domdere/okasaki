module Data.OrderedTree where

-- | Chapter 3 of "Purely Functional Data Structures" discusses a couple of different
-- variations of LeftistHeaps, the vanilla one and the "weight biased" one,
-- which only differ in their definition of the rank, also you could implement
-- one which caches the rank at each node and another which calculates it dynamically

class OrderedTree h where
    rank :: h a -> Int
    empty :: h a
    isEmpty :: h a -> Bool
    getElement :: h a -> Maybe a
    leftTree :: h a -> Maybe (h a)
    rightTree :: h a -> Maybe (h a)
    makeTree :: a -> h a -> h a -> h a

