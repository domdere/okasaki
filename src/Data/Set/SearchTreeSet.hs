module Data.Set.SearchTreeSet where

import Data.SearchTree
import Data.Set

-- | A wrapper that gives a SearchTree a Set instance
newtype SearchTreeSet t a = SearchTreeSet
    {   fromSearchTreeSet :: t a
    } deriving (Show, Eq)

instance (SearchTree t) => Set (SearchTreeSet t) where
    empty = SearchTreeSet emptyTree
    insert x s = SearchTreeSet $ treeInsertIfMissing x $ fromSearchTreeSet s
    remove x s = SearchTreeSet $ removedFromTree x $ fromSearchTreeSet s
    member x s = treeMember x $ fromSearchTreeSet s
