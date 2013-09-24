{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.LeftistHeap
    (   LeftistHeap
    ,   leftistProperty
    ,   rank
    ,   singletonHeap
    ,   fromList
    ) where

import Control.Applicative hiding (empty)
import Data.Function
import Data.Maybe

import Data.Heap
import Data.OrderedTree

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (LeftistHeap a) where arbitrary = fromList <$> arbitrary

-- | from Chapter 3 of "Purely Functional Data Structures" by Chris Okasaki
-- "Leftist Heaps are heap ordered binary trees that satisfy the leftist property:
-- the rank of any left child is at least as large as the rank of its right sibling,
-- where the rank of a tree is defined to be the length of its right spine."
--
-- The Data Constructors for the LeftitstHeap are not exported as Tree contains
-- the rank, and everything relies on the correctness, and users could corrupt it
data LeftistHeap a =
        EmptyHeap
    |   Tree Int a (LeftistHeap a) (LeftistHeap a) deriving (Show, Eq)

-- | retrieves the topmost element of the heap
-- prop> (\x -> getElement' (fromList [x]) == Just x) (x :: Int)
--
-- >>> getElement' EmptyHeap
-- Nothing
--
getElement' :: LeftistHeap a -> Maybe a
getElement' EmptyHeap       = Nothing
getElement' (Tree _ x _ _)  = Just x

-- | Get the left tree at a node
--
-- >>> leftTree' EmptyHeap
-- Nothing
--
-- >>> toList <$> leftTree' (fromList [1,2,3,4,5,6,7,8,9,10])
-- Just [3,4,5,6,7,8,9,10]
--
leftTree' :: LeftistHeap a -> Maybe (LeftistHeap a)
leftTree' EmptyHeap         = Nothing
leftTree' (Tree _ _ a _)    = Just a

-- | Get the right tree at a node
--
-- >>> rightTree' EmptyHeap
-- Nothing
--
-- >>> toList <$> rightTree' (fromList [1,2,3,4,5,6,7,8,9,10])
-- Just [2]
--
rightTree' :: LeftistHeap a -> Maybe (LeftistHeap a)
rightTree' EmptyHeap        = Nothing
rightTree' (Tree _ _ _ b)   = Just b

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
rank' :: LeftistHeap a -> Int
rank' EmptyHeap      = 0
rank' (Tree x _ _ _) = x

-- | this version of rank calcs the rank from the definition of the rank, rather than
-- caching it, rank would obviously be O(1) while rank' would be something like O(log n) (see Exercise 3.1)
--
-- rank' should be equivalent to rank
-- prop> (\heap -> rank heap == rank' heap) (heap :: LeftistHeap Int)
--
rank'' :: LeftistHeap a -> Int
rank'' EmptyHeap                 = 0
rank'' (Tree _ _ _ rightHeap)    = rank'' rightHeap + 1

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
makeTree' :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeTree' x a b
    | rank a < rank b = Tree (rank a + 1) x b a
    | otherwise       = Tree (rank b + 1) x a b

-- | the empty heap
empty' :: LeftistHeap a
empty' = EmptyHeap

-- | True if empty False otherwise
isEmpty' :: LeftistHeap a -> Bool
isEmpty' EmptyHeap  = True
isEmpty' _          = False

instance OrderedTree LeftistHeap where
    rank = rank'
    empty = empty'
    isEmpty = isEmpty'
    getElement = getElement'
    leftTree = leftTree'
    rightTree = rightTree'
    makeTree = makeTree'

-- | verifies the leftist property...
leftistProperty :: (Heap h, OrderedTree h) => h a -> Bool
leftistProperty heap = fromMaybe True $ maybeLeftistProperty heap
    where
        maybeLeftistProperty :: (Heap h, OrderedTree h) => h a -> Maybe Bool
        maybeLeftistProperty heap' = do
            leftHeap <- leftTree heap'
            rightHeap <- rightTree heap'
            return $ rank leftHeap >= rank rightHeap


-- | Merge two leftist heaps
-- two leftist heaps can be merged by merging their
-- right spines as you would merge 2 sorted lists and then swapping
-- the children of the nodes along this path to restore the leftist
-- property (which is what makeTree does)
-- Its O(log n)
--
merge' :: (Ord a, OrderedTree h) => h a -> h a -> h a
merge' w z = fromMaybe (if isEmpty z then w else z) $ maybeMerge w z
    where
        maybeMerge :: (Ord a, OrderedTree h) => h a -> h a -> Maybe (h a)
        maybeMerge w' z' = do
            x <- getElement w'
            y <- getElement z'
            a <- leftTree w'
            b <- rightTree w'
            c <- leftTree z'
            d <- rightTree z'
            return $ if x > y
            then
                makeTree y c (merge' w' d)
            else
                makeTree x a (merge' b z')

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
singletonHeap :: (OrderedTree h) => a -> h a
singletonHeap x = makeTree x empty empty

-- | insert an elt by merging the heap with the singleton heap
-- of the new element.
-- O(log n)
insert' :: (Ord a, OrderedTree h) => a-> h a -> h a
insert' x = merge' $ singletonHeap x

-- | find the minimum of the heap
-- O(1) time
findMin' :: (OrderedTree h) => h a -> Maybe a
findMin' = getElement

-- | delete the minimum,
-- O(log n) time
deleteMin' :: (Ord a, OrderedTree h) => h a -> h a
deleteMin' h = fromMaybe h $ maybeDeleteMin h
    where
        maybeDeleteMin :: (Ord a, OrderedTree h) => h a -> Maybe (h a)
        maybeDeleteMin heap = do
            a <- leftTree heap
            b <- rightTree heap
            return $ merge' a b

newtype OrderedHeap h a = OrderedHeap { fromOrderedHeap :: h a } deriving (Show, Eq, OrderedTree)

instance (OrderedTree h) => Heap (OrderedHeap h) where
    insert x = OrderedHeap . insert' x . fromOrderedHeap
    merge w z = OrderedHeap $ (merge' `on` fromOrderedHeap) w z
    findMin = findMin' . fromOrderedHeap
    deleteMin = OrderedHeap <$> (deleteMin' . fromOrderedHeap)

-- Exercises

-- | Exercise 3.1: prove that the right spine of a leftist heap contains at most floor (log (n + 1))
-- elements.
--
-- TODO.

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
fromList :: (Ord a, Heap h) => [a] -> h a
fromList [] = empty
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
toList :: (Ord a, Heap h) => h a -> [a]
toList heap = case findMin heap of
    Nothing -> []
    Just x -> x : toList (deleteMin heap)

-- | Exercise 3.4
-- A **weight biased** leftist heap is one where the rank is instead defined by the number of elements contained in the tree.
weightBiasedRank :: LeftistHeap a -> Int
weightBiasedRank EmptyHeap              = 0
weightBiasedRank (Tree _ _ left right)  = weightBiasedRank left + weightBiasedRank right

-- | weight biased leftist heaps have the following revised leftist property, true for all weight biased leftist heaps
weightBiasedLeftistProperty :: LeftistHeap a -> Bool
weightBiasedLeftistProperty EmptyHeap               = True
weightBiasedLeftistProperty (Tree _ _ left right)   = weightBiasedRank left >= weightBiasedRank right

-- | Exercise 3.4 (a) Prove that in a weight biased leftist heap, the right spine contains at most floor (log (n + 1)) elements,
-- or if r is the number of elements in the right spine, then
--      r <= floor(log (n + 1))
--      r <= log(n + 1) (r <- Z, and there are no more integers in [floor(log (n + 1)), log(n + 1)])
--      2^r <= n + 1
--      n => 2^r - 1
-- (WIP)
