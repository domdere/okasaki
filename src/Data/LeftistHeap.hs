module Data.LeftistHeap 
    (   LeftistHeap(..)
    ,   leftistProperty
    ,   rank
    ,   singletonHeap
    ,   fromList
    ) where

import Data.Function

import Data.Heap

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (LeftistHeap a) where arbitrary = fromList <$> arbitrary

-- | from Chapter 3 of "Purely Functional Data Structures" by Chris Okasaki
-- "Leftist Heaps are heap ordered binary trees that satisfy the leftist property:
-- the rank of any left child is at least as large as the rank of its right sibling,
-- where the rank of a tree is defined to be the length of its right spine."
--
data LeftistHeap a =
        EmptyHeap
    |   Tree Int a (LeftistHeap a) (LeftistHeap a) deriving (Show, Eq)

-- | verifies the leftist property...
leftistProperty :: LeftistHeap a -> Bool
leftistProperty EmptyHeap = True
leftistProperty (Tree _ _ left right) = rank left >= rank right

-- | The rank of a tree is defined to be the length of its right spine
-- In Okasaki's implementation the rank is pre calced and stored
-- on the node.
--
-- >>> rank EmptyHeap
-- 0
--
-- >>> rank (fromList [5,7,3,8,2,3,9,10])
-- 2
--
rank :: LeftistHeap a -> Int
rank EmptyHeap      = 0
rank (Tree x _ _ _) = x

-- | a helper function Okasaki uses as part of the merge function
-- for the LeftistHeap.
-- It takes the element, and two leftist heaps,
-- and constructs the node which contains the element
-- and the two given heaps as children, such that the leftist 
-- property is satisfied.
-- O(1)
--
-- prop> (\x a b -> leftistProperty (makeTree x a b)) (x :: Int) (a :: LeftistHeap Int) (b :: LeftistHeap Int)
--
-- >>> makeTree 3 EmptyHeap (makeTree 1 EmptyHeap EmptyHeap)
-- Tree 1 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap
--
-- >>> makeTree 3 (makeTree 1 EmptyHeap EmptyHeap) EmptyHeap
-- Tree 1 3 (Tree 1 1 EmptyHeap EmptyHeap) EmptyHeap
--
makeTree :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeTree x a b
    | rank a < rank b = Tree (rank a + 1) x b a
    | otherwise       = Tree (rank b + 1) x a b

-- | Merge two leftist heaps
-- two leftist heaps can be merged by merging their
-- right spines as you would merge 2 sorted lists and then swapping
-- the children of the nodes along this path to restore the leftist
-- property (which is what makeTree does)
-- Its O(log n)
--
merge' :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge' heap EmptyHeap = heap
merge' EmptyHeap heap = heap
merge' w@(Tree _ x a b) z@(Tree _ y c d)
    | x > y     = makeTree y c (merge' w d)
    | otherwise = makeTree x a (merge' b z)

-- | create the singleton heap from a given element:
--
-- The min of the singleton heap must be the element we put in.
-- prop> (\x -> findMin (singletonHeap x) == (Just x)) (x :: Int)
--
-- rank of any singleton must be 1.
-- prop> (\x -> rank (singletonHeap x) == 1) (x :: Int)
--
-- singleton must not be empty..
-- prop> (\x -> not (isEmpty (singletonHeap x))) (x :: Int)
--
-- however, if you delete the min then it should become empty...
-- prop> (\x -> isEmpty (deleteMin (singletonHeap x))) (x :: Int)
--
singletonHeap :: a -> LeftistHeap a
singletonHeap x = Tree 1 x EmptyHeap EmptyHeap

-- | insert an elt by merging the heap with the singleton heap
-- of the new element.
-- O(log n)
insert' :: Ord a => a-> LeftistHeap a -> LeftistHeap a
insert' x = merge' $ singletonHeap x

-- | find the minimum of the heap
-- O(1) time
findMin' :: LeftistHeap a -> Maybe a
findMin' EmptyHeap      = Nothing
findMin' (Tree _ x _ _) = Just x

-- | delete the minimum,
-- O(log n) time
deleteMin' :: (Ord a) => LeftistHeap a -> LeftistHeap a
deleteMin' EmptyHeap        = EmptyHeap
deleteMin' (Tree _ _ a b)   = merge' a b

-- | the empty heap
empty' :: LeftistHeap a
empty' = EmptyHeap

-- | True if empty False otherwise
isEmpty' :: LeftistHeap a -> Bool
isEmpty' EmptyHeap  = True
isEmpty' _          = False

instance Heap LeftistHeap where
    empty = empty'
    isEmpty = isEmpty'
    insert = insert'
    merge = merge'
    findMin = findMin'
    deleteMin = deleteMin'

-- Exercise

-- | Exercise 3.2: define insert directly rather than through a call
-- to merge.
--
addTo :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a
x `addTo` EmptyHeap = singletonHeap x
x `addTo` h@(Tree _ y a b)
    | x > y     = makeTree y a (x `addTo` b)
    | otherwise = makeTree x EmptyHeap h

-- | Exercise 3.3 : Implement a function fromList that produces a
-- LeftistHeap from an unordered list of elements by first
-- converting each element into a singleton heap and then merging the heaps
-- until only one remains, dont use foldl/foldr and instead merge
-- the heaps in ceil(log n) passes.  Show that fromList takes O(n) time..
--
-- prop> (\xs -> leftistProperty (fromList xs)) (xs :: [Int])
--
-- >>> fromList [14,5,7,3,2,7]
-- Tree 2 2 (Tree 1 7 EmptyHeap EmptyHeap) (Tree 1 3 (Tree 2 5 (Tree 1 7 EmptyHeap EmptyHeap) (Tree 1 14 EmptyHeap EmptyHeap)) EmptyHeap)
--
-- >>> fromList []
-- EmptyHeap
--
fromList :: Ord a => [a] -> LeftistHeap a
fromList [] = EmptyHeap
fromList (x:[]) = singletonHeap x
fromList xs = (merge `on` fromList) firstHalf secondHalf
    where
        (firstHalf, secondHalf) = (length xs `quot` 2)`splitAt` xs

-- | converts a LeftistHeap into a sorted list
--
-- >>> (toList .fromList) [14,5,7,3,2,7]
-- [2,3,5,7,7,14]
--
-- >>> (toList .fromList) [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
--
toList :: (Ord a) => LeftistHeap a -> [a]
toList heap = case findMin heap of 
    Nothing -> []
    Just x -> x : toList (deleteMin heap)
