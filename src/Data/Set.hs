module Data.Set
    ( Set(..)
    ) where

class Set s where
    empty :: s a
    insert :: (Ord a) => a -> s a -> s a
    remove :: (Ord a) => a -> s a -> s a
    member :: (Ord a) => a -> s a -> Bool

-- Invariants

-- | No dupes allowed in sets.
noDupes :: (Set s, Ord a) => a -> s a -> Bool
noDupes x set = not $ member x $ x `remove` (x `insert` (x `insert` set))
