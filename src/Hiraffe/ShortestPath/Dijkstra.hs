--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.ShortestPath.Dijkstra
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of Dijkstra's shortest path algorithm.
--
--------------------------------------------------------------------------------
module Hiraffe.ShortestPath.Dijkstra
  ( shortestPaths
  , WithPath(..)
  ) where

-- import Control.Lens

import Data.Foldable
import Data.Function (on)
import Data.PSQueue (Binding(..))
import Data.PSQueue qualified as PSQueue
import HGeometry.Unbounded

--------------------------------------------------------------------------------

-- | Dijkstra's algorithm for computing the shortest paths from the
-- given source node to all other nodes in the unweighted graph
--
-- O(V\log V + E\log V)
shortestPaths           :: forall graph adjs v w.
                           ( Foldable graph, Foldable adjs, Ord v, Ord w, Monoid w)
                        => (v -> v -> Top w) -- ^ weight function
                        -> v -- ^ source
                        -> graph (v, adjs v)    -- ^ adjacency graph
                        -> [(v, Top w)]
shortestPaths weight s graph = go . initializeQueue $ graph
  where
    initializeQueue :: graph (v, adjs v) -> Queue adjs v w
    initializeQueue = PSQueue.adjust (fmap (const mempty)) s
                    . PSQueue.fromList
                    . map (\(u,ns) -> u :-> WithNeighbours Top ns)
                    . toList

    go       :: Queue adjs v w -> [(v,Top w)]
    go queue = case PSQueue.minView queue of
      Nothing                                      -> []
      Just (u :-> WithNeighbours du neighs,queue') -> (u, du) : go (relaxAll u du queue' neighs)

    relaxAll      :: v -> Top w -> Queue adjs v w -> adjs v -> Queue adjs v w
    relaxAll u du = foldr (\v -> decreasePrio v $ du <> weight u v)

--------------------------------------------------------------------------------

type Queue adjs v r = PSQueue.PSQ v (WithNeighbours (adjs v) (Top r))

-- | Decrease the key
decreasePrio     :: (Ord k, Ord p, Functor f, Ord (f p)) => k -> p
                 -> PSQueue.PSQ k (f p) -> PSQueue.PSQ k (f p)
decreasePrio k p = PSQueue.adjust (fmap (min p)) k


-- | Attach neighbours to the priorities
data WithNeighbours vs p = WithNeighbours { thePrio  :: !p
                                          , _neighs  :: vs
                                          }
                         deriving (Functor)

instance Eq p => Eq (WithNeighbours vs p) where
  (==) = (==) `on` thePrio
instance Ord p => Ord (WithNeighbours vs p) where
  compare = compare `on` thePrio

--------------------------------------------------------------------------------

data WithPath f v r = WithPath { path  :: f v
                               , dist  :: !r
                               } deriving (Show)

instance Eq r => Eq (WithPath f v r) where
  (==) = (==) `on` dist
instance Ord r => Ord (WithPath f v r) where
  compare = compare `on` dist

instance (Semigroup r, Semigroup (f v)) => Semigroup (WithPath f v r) where
  (WithPath p d) <> (WithPath p' d') = WithPath (p <> p') (d <> d')

instance (Monoid r, Monoid (f v)) => Monoid (WithPath f v r) where
  mempty = WithPath mempty mempty


--------------------------------------------------------------------------------
