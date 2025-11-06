module TreeSet
  ( Set
  , empty
  , insert
  , member
  , fromList
  ) where

import Data.List (sort)

data Set a = Empty
           | Node (Set a) a (Set a)
  deriving (Eq, Show)

empty :: Set a
empty = Empty

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node Empty x Empty
insert x (Node l y r)
  | x == y    = Node l y r
  | x < y     = Node (insert x l) y r
  | otherwise = Node l y (insert x r)

member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member x (Node l y r)
  | x == y    = True
  | x < y     = member x l
  | otherwise = member x r

-- Build a balanced tree from a list: sort, remove duplicates, then build
fromList :: Ord a => [a] -> Set a
fromList xs = buildBalanced (uniqueSorted (sort xs))
  where
    uniqueSorted [] = []
    uniqueSorted (y:ys) = y : uniqueSorted (dropWhile (== y) ys)

    buildBalanced [] = Empty
    buildBalanced ys =
      let n = length ys
          mid = n `div` 2
          (l, m:r) = splitAt mid ys
      in Node (buildBalanced l) m (buildBalanced r)
