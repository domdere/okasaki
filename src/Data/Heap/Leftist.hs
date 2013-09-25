module Data.Heap.Leftist
    (   Leftist(..)
    ,   LeftistHeap(..)
    ,   leftistProperty
    ,   leftistRank
    ,   findLeftistMin
    ,   deleteLeftistMin
    ,   emptyLeftist
    ,   isEmptyLeftist
    ,   singletonLeftistHeap
    ,   mergeLeftist
    ,   addTo
    ) where

import Data.Function (on)
import Data.Maybe (fromMaybe)

-- $setup
-- >>> import Data.Heap
-- >>> import Data.Heap.Leftist.HeightBiased
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype TestHeap = TestHeap (HBLHeap Int)
-- >>> instance Arbitrary TestHeap where arbitrary = (TestHeap . fromList) <$> arbitrary

-- | from Chapter 3 of "Purely Functional Data Structures" by Chris Okasaki
-- "Leftist Heaps are heap ordered binary trees that satisfy the leftist property:
-- the rank of any left child is at least as large as the rank of its right 
-- sibling".  Different variants of the Leftist Heap differ in their
-- definition of `rank'.
--

-- | All Leftist Heaps can be represented like so
-- where the arguments to Tree are the (precalced)
-- rank, minimum element, left subtree, right subtree
-- Hence, all LeftistHeaps can be converted to
-- this representation...
data LeftistHeap a =
        EmptyHeap
    |   Tree Int a (LeftistHeap a) (LeftistHeap a) deriving (Show, Eq)

-- | A representation of a Leftist heap must provide a definition for
-- its concept of rank (via toLeftist)
-- and define how to fuse leftist trees together
-- at a node (and pre calc the rank)
class Leftist h where
    toLeftist :: h a -> LeftistHeap a
    fromLeftist :: LeftistHeap a -> h a
    combineTrees :: Eq a => a -> h a -> h a -> h a

instance Eq a => Ord (LeftistHeap a) where
    compare = compare `on` leftistRank

-- Properties

-- | verifies the leftist property...
leftistProperty :: (Leftist h) => h a -> Bool
leftistProperty heap =
    case toLeftist heap of
        EmptyHeap -> True
        Tree _ _ leftHeap rightHeap -> leftistRank leftHeap >= leftistRank rightHeap

-- | retrieves the pre calced rank:
leftistRank :: LeftistHeap a -> Int
leftistRank EmptyHeap = 0
leftistRank (Tree r _ _ _) = r

-- | retrieves the topmost element of the heap
-- prop> (\x -> findLeftistMin (toLeftist (fromList [x] :: HBLHeap Int)) == Just x) (x :: Int)
--
-- >>> findLeftistMin EmptyHeap
-- Nothing
--
findLeftistMin :: LeftistHeap a -> Maybe a
findLeftistMin EmptyHeap       = Nothing
findLeftistMin (Tree _ x _ _)  = Just x

-- | the empty heap
emptyLeftist :: LeftistHeap a
emptyLeftist = EmptyHeap

-- | True if empty False otherwise
isEmptyLeftist :: LeftistHeap a -> Bool
isEmptyLeftist EmptyHeap    = True
isEmptyLeftist _            = False

-- | create the singleton heap from a given element:
--
-- The min of the singleton heap must be the element we put in.
-- prop> (\x -> findMin (singletonHeap x :: HBLHeap Int) == (Just x)) (x :: Int)
--
-- rank of any singleton must be 1.
-- prop> (\x -> rank (singletonHeap x :: HBLHeap Int) == 1) (x :: Int)
--
-- singleton must not be empty..
-- prop> (\x -> not (isEmpty (singletonHeap x :: HBLHeap Int))) (x :: Int)
--
singletonLeftistHeap :: a -> LeftistHeap a
singletonLeftistHeap x = Tree 1 x EmptyHeap EmptyHeap

-- | delete the minimum,
-- Same time complexity as merge
deleteLeftistMin :: (Ord a, Leftist h) => h a -> h a
deleteLeftistMin h =
    case toLeftist h of
        EmptyHeap -> h
        Tree _ _ a b -> (mergeLeftist `on` fromLeftist) a b


-- | Merge two leftist heaps
-- two leftist heaps can be merged by merging their
-- right spines as you would merge 2 sorted lists and then swapping
-- the children of the nodes along this path to restore the leftist
-- property (which is what makeTree does)
-- Its O(log n)
--
-- merge' is not associative under the strict equality,
-- so it cannot serve as a suitable mappend
-- for a Monoid Instance unless an Eq instance is provided
-- that defines two leftist heaps to be equivalent if toList
-- on the operands results on the same list.
--
-- i.e (==) = (==) `on` toList
--
mergeLeftist :: (Ord a, Leftist h) => h a -> h a -> h a
mergeLeftist w' z' =
    case (toLeftist w', toLeftist z') of
        (EmptyHeap, _) -> z'
        (_, EmptyHeap) -> w'
        (Tree _ x a b, Tree _ y c d) ->
            if x > y then
                combineTrees y (fromLeftist c) (mergeLeftist w' (fromLeftist d))
            else
                combineTrees x (fromLeftist a) (mergeLeftist (fromLeftist b) z')

-- | Exercise 3.2: define insert directly rather than through a call
-- to merge.
--
addTo :: (Ord a, Leftist f) => a -> f a -> f a
x `addTo` h =
    case toLeftist h of
        EmptyHeap       -> fromLeftist $ singletonLeftistHeap x
        (Tree _ y a b)  ->
            if x > y then
                combineTrees y (fromLeftist a) (x `addTo` fromLeftist b)
            else
                combineTrees x (fromLeftist EmptyHeap) h


