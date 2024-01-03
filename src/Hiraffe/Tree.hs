--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.BFS
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Hiraffe.Tree
  ( pathsTo
  , findPaths
  , labelPrefixWith
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Tree

--------------------------------------------------------------------------------

-- | Get all paths to the particular element in the tree.
pathsTo   :: Eq a => a -> Tree a -> [NonEmpty a]
pathsTo x = findPaths (== x)

-- | All paths to the nodes satisfying the predicate.
findPaths   :: (a -> Bool) -> Tree a -> [NonEmpty a]
findPaths p = go
  where
    go (Node x chs) = case foldMap go chs of
                        []    | p x       -> [x:|[]]
                              | otherwise -> []
                        paths | p x       -> (x:|[]) : map (x NonEmpty.<|) paths
                              | otherwise ->           map (x NonEmpty.<|) paths

-- -- |
-- foldTree   :: (a -> [r] -> r) -> Tree a -> r
-- foldTree f = go
--   where
--     go (Node x chs) = f x (map go chs)

-- -- distances :: Tree a -> [(a,Int)]
-- -- distances = mconcat . zipWith (\d lvl -> (,d) <$> lvl) [0..] . levels

-- -- | Label all nodes with their distances from the root.
-- labelWithDistances :: Integral i => Tree a -> Tree (a, i)
-- labelWithDistances = labelPrefixWith 0 (const (1 + ))



-- | Label all nodes with their "distances" from the root.
--
-- >>> labelPrefixWith 0 (const succ) $ Node "r" [Node "a" [], Node "b" [Node "foo" []], Node "c" []]
-- Node {rootLabel = ("r",0), subForest = [Node {rootLabel = ("a",1), subForest = []},Node {rootLabel = ("b",1), subForest = [Node {rootLabel = ("foo",2), subForest = []}]},Node {rootLabel = ("c",1), subForest = []}]}
labelPrefixWith     :: d -> (a -> d -> d) -> Tree a -> Tree (a, d)
labelPrefixWith z f = go z
  where
    go d (Node x chs) = Node (x,d) [ go (f x d) ch | ch <- chs ]
