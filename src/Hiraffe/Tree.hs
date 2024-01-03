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
