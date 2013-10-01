module Data.Set
    ( Set(..)
    ) where

-- $setup
-- >>> import Data.Set.SearchTreeSet
-- >>> import Data.SearchTree.Binary
-- >>> import qualified Data.SearchTree as ST
-- >>> import Control.Applicative hiding (empty)
-- >>> import Test.QuickCheck
-- >>> newtype TestSet a = TestSet (SearchTreeSet BinaryTree a) deriving (Show, Eq)
--
-- obvously can't use this Arbitrary
-- instance to test fromList
-- >>> instance (Arbitrary a, Ord a) => Arbitrary (TestSet a) where arbitrary = (TestSet . SearchTreeSet . ST.fromList) <$> arbitrary

class Set s where
    empty :: s a
    insert :: (Ord a) => a -> s a -> s a
    remove :: (Ord a) => a -> s a -> s a
    member :: (Ord a) => a -> s a -> Bool

-- Invariants

-- | No dupes allowed in sets.
noDupes :: (Set s, Ord a) => a -> s a -> Bool
noDupes x set = not $ member x $ x `remove` (x `insert` (x `insert` set))

-- | Set functions

-- | creates a set from a list of orderables:
--
-- a property is that for all the elements x in the
-- input list, x is a member of the tree
-- prop> (\xs -> all (flip member ((fromList xs) :: SearchTreeSet BinaryTree Int)) xs) (xs :: [Int])
--
fromList :: (Set s, Ord a) => [a] -> s a
fromList = foldl (flip insert) empty


