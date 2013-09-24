module Data.Heap.WeightBiasedLeftist
    (   WeightBiasedLeftistHeap
    ) where

import Data.Heap

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (WeightBiasedLeftistHeap a) where arbitrary = fromList <$> arbitrary

-- | see Data.Heap.Leftist for a definition of
-- leftist heaps
data WeightBiasedLeftistHeap a =
        EmptyHeap
    |   Tree Int a (WeightBiasedLeftistHeap a) (WeightBiasedLeftistHeap a) deriving (Show, Eq)


