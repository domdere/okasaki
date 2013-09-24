module Data.Heap 
    (   Heap(..)
    ,   minOfEmptyHeapIsNothing
    ,   deleteMinOfEmptyTreeGivesEmptyTree
    ) where

import Data.OrderedTree

import Data.Maybe


-- | Chapter 3 of C.Okasaki's "Purely Functional Data Structures" deals with some Heaps/Priority Queues
-- he defines the following typeclass
--
class Heap h where
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a

    findMin :: Ord a => h a -> Maybe a
    deleteMin :: Ord a => h a -> h a

-- He specifies the following properties for his Heap functions:

-- | The minimum of an empty heap is nothing (I added this)
minOfEmptyHeapIsNothing :: (Heap h, Ord a) => h a -> Bool
minOfEmptyHeapIsNothing emptyHeap = isNothing $ findMin emptyHeap

-- | deleting the minimum of an empty tree results in the empty tree...
deleteMinOfEmptyTreeGivesEmptyTree :: (Eq (h a), Heap h, Ord a) => h a -> Bool
deleteMinOfEmptyTreeGivesEmptyTree emptyHeap = deleteMin emptyHeap == emptyHeap
