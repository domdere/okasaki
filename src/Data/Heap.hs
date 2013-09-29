module Data.Heap 
    (   Heap(..)
    ,   insert
    ,   minOfEmptyHeapIsNothing
    ,   heapOrder
    ,   fromList
    ,   toList
    ) where

import Data.Function
import Data.Maybe

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.Heap.Leftist
-- >>> import Data.Heap.Leftist.HeightBiased
-- >>> import Test.QuickCheck
-- >>> newtype TestHeap = TestHeap (HBLHeap Int)
-- >>> instance Arbitrary TestHeap where arbitrary = (TestHeap . fromList) <$> arbitrary

-- | Chapter 3 of C.Okasaki's "Purely Functional Data Structures" deals with some Heaps/Priority Queues
-- he defines the following typeclass
--
class Heap h where
    singletonHeap :: a -> h a
    empty :: h a
    isEmpty :: h a -> Bool

    merge :: Ord a => h a -> h a -> h a

    findMin :: Ord a => h a -> Maybe a
    deleteMin :: Ord a => h a -> h a

-- | Heap Invariants

-- | The most obvious heap invariant is heap order
heapOrder :: (Heap h, Ord a) => h a -> Bool
heapOrder h = isSorted $ toList h
    where
        isSorted []         = True
        isSorted (_:[])     = True
        isSorted (x:y:xs)   = x <= y && isSorted (y:xs)

-- He specifies the following properties for his Heap functions:

-- | The minimum of an empty heap is nothing (I added this)
minOfEmptyHeapIsNothing :: (Heap h, Ord a) => h a -> Bool
minOfEmptyHeapIsNothing emptyHeap = isNothing $ findMin emptyHeap

-- The following function are derived from the type class methods

-- | insert an elt by merging the heap with the singleton heap
-- of the new element.
-- same time complexity as merge
insert :: (Ord a, Heap h) => a-> h a -> h a
insert x = merge $ singletonHeap x

-- | Exercise 3.3 : Implement a function fromList that produces a
-- (Leftist)Heap from an unordered list of elements by first
-- converting each element into a singleton heap and then merging the heaps
-- until only one remains, dont use foldl/foldr and instead merge
-- the heaps in ceil(log n) passes.  Show that fromList takes O(n) time..
--
-- >>> fromList [14,5,7,3,2,7] :: HBLHeap Int
-- HBLHeap (Tree 2 2 (Tree 1 7 EmptyHeap EmptyHeap) (Tree 1 3 (Tree 2 5 (Tree 1 7 EmptyHeap EmptyHeap) (Tree 1 14 EmptyHeap EmptyHeap)) EmptyHeap))
--
-- >>> fromList [] :: HBLHeap Int
-- HBLHeap EmptyHeap
--
fromList :: (Ord a, Heap h) => [a] -> h a
fromList [] = empty
fromList (x:[]) = singletonHeap x
fromList xs = (merge `on` fromList) firstHalf secondHalf
    where
        (firstHalf, secondHalf) = (length xs `quot` 2)`splitAt` xs

-- | converts a Heap into a sorted list
--
-- >>> (toList . (fromList :: [Int] -> HBLHeap Int)) [14,5,7,3,2,7]
-- [2,3,5,7,7,14]
--
-- >>> (toList . (fromList :: [Int] -> HBLHeap Int)) [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
--
toList :: (Ord a, Heap h) => h a -> [a]
toList heap = case findMin heap of
    Nothing -> []
    Just x -> x : toList (deleteMin heap)


