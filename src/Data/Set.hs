{-
Copyright (c) 2013 Dom De Re

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

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


