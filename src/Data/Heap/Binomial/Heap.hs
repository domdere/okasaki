module Data.Heap.Binomial.Heap
    (
    ) where

import Data.Heap
import Data.Heap.Binomial.Tree

import Data.Function
import Data.Maybe

-- $setup
--
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
--
-- >>> newtype NonEmpty a = NonEmpty (BinomialHeap a) deriving (Show, Eq)
--
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (BinomialHeap a) where arbitrary = fromList <$> arbitrary
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (NonEmpty a) where arbitrary = do; x <-arbitrary; xs <- arbitrary; return (NonEmpty (fromList (x:xs)));

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
-- prop> (heapOrder . singletonHeap') (x :: Int)
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
-- >>> heapOrder(empty' :: BinomialHeap Int)
-- True
--
empty' :: BinomialHeap a
empty' = BinomialHeap []

-- | checks to see if there any elements in the heap
--
-- prop> (\x xs -> not (isEmpty' (fromList (x:xs)))) (x :: Int) (xs :: [Int])
--
-- >>> isEmpty' (empty' :: BinomialHeap Int)
-- True
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
-- must perserve the heap order invariant:
-- prop> ((heapOrder .) . merge') (h :: BinomialHeap Int) (h' :: BinomialHeap Int)
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
-- prop> (\x xs -> findMin' (fromList (x:xs)) == Just (minimum (x:xs))) (x :: Int) (xs :: [Int])
--
-- >>> findMin' (empty' :: BinomialHeap Int)
-- Nothing
--
-- >>> findMin' (singletonHeap' 4)
-- Just 4
--
-- >>> findMin' ((fromList [5,8,6,3,7,9,10]) :: BinomialHeap Int)
-- Just 3
--
findMin' :: (Ord a) => BinomialHeap a -> Maybe a
findMin' h = (\(BinomialTree _ x _, _) -> x) `fmap` removeMinTree h

-- | Exercise 3.5
-- define findMin directly rather than through a call to `removeMinTree`.
--
-- prop> (\x xs -> findMin'' (fromList (x:xs)) == Just (minimum (x:xs))) (x :: Int) (xs :: [Int])
--
-- >>> findMin'' (empty' :: BinomialHeap Int)
-- Nothing
--
-- >>> findMin'' (singletonHeap' 4)
-- Just 4
--
-- >>> findMin'' ((fromList [5,8,6,3,7,9,10]) :: BinomialHeap Int)
-- Just 3
--
findMin'' :: (Ord a) => BinomialHeap a -> Maybe a
findMin'' (BinomialHeap []) = Nothing
findMin'' (BinomialHeap (BinomialTree _ x _:[])) = Just x
findMin'' (BinomialHeap (BinomialTree _ x _:ts)) = do
    x' <- findMin'' (BinomialHeap ts)
    return $ x `min` x'

-- | Deletes the minimum element
--
-- Due to the heap order each time you call deleteMin', findMin' should return something larger than before
-- prop> (\x x' xs -> (deleteMin' >>= ((>=) `on` findMin')) (fromList (x:x':xs))) (x :: Int) (x' :: Int) (xs :: [Int])
--
-- >>> deleteMin' empty' == (empty' :: BinomialHeap Int)
-- True
--
-- >>> deleteMin' (BinomialHeap [BinomialTree 0 0 [],BinomialTree 2 1 [BinomialTree 1 8 [BinomialTree 0 8 []],BinomialTree 0 1 []]])
-- BinomialHeap [BinomialTree 2 1 [BinomialTree 1 8 [BinomialTree 0 8 []],BinomialTree 0 1 []]]
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

    findMin     = findMin''
    deleteMin   = deleteMin'

-- Helper Functions

-- | Returns the tree containing the minimum element and
-- the heap with that tree removed
--
removeMinTree :: (Ord a) => BinomialHeap a -> Maybe (BinomialTree a, BinomialHeap a)
removeMinTree (BinomialHeap [])                             = Nothing
removeMinTree (BinomialHeap (t@(BinomialTree _ x _):ts))    = case removeMinTree (BinomialHeap ts) of
    Nothing -> Just (t, BinomialHeap [])
    Just (t'@(BinomialTree _ x' _), BinomialHeap ts') ->
        Just (if x < x' then
            (t, BinomialHeap ts)
        else
            (t', BinomialHeap (t:ts')))
