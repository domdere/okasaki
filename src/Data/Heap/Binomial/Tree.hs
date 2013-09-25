module Data.Heap.Binomial.Tree
    (   BinomialTree
    ,   treeRank
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
data BinomialTree a = BinomialTree a [BinomialTree a] deriving (Show, Eq)

-- | the tree rank is defined as above, hence it can be calculated
-- from the length of the list of children.
-- runs in O(r) time
treeRank :: BinomialTree a -> Int
treeRank (BinomialTree _ xs) = length xs

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
singletonTree x = BinomialTree x []

-- | the link operation for two trees, only returns a result
-- when the two trees have the same rank, returns Nothing otherwise..
--
-- >>> link (singletonTree 3) (singletonTree 4)
-- Just (BinomialTree 3 [BinomialTree 4 []])
--
-- >>> link (singletonTree 5) (singletonTree 2)
-- Just (BinomialTree 2 [BinomialTree 5 []])
--
link :: (Ord a) => BinomialTree a -> BinomialTree a -> Maybe (BinomialTree a)
link w@(BinomialTree x ws) z@(BinomialTree y zs)
    | ((/=) `on` treeRank) w z  = Nothing
    | x < y                     = Just $ BinomialTree x (z:ws)
    | otherwise                 = Just $ BinomialTree y (w:zs)

-- Invariants

-- | The children of the binomial trees are expected to be in order
-- decreasing rank, hence this property becomes an invariant...
--
decreasingRank :: BinomialTree a -> Bool
decreasingRank (BinomialTree _ children) =
    isStrictlyDecreasing $ treeRank `fmap` children

-- | elements are to be stored in heap order..
--
eltsInHeapOrder :: (Ord a) => BinomialTree a -> Bool
eltsInHeapOrder t = isMonotonicallyIncreasing $ toList' t

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
toList' (BinomialTree x []) = [x]
toList' (BinomialTree x ts) = x : foldr (\t xs -> toList' t ++ xs) [] ts
