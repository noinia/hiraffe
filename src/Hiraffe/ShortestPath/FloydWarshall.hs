--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.ShortestPath.FloydWarshall
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- Implementation of Floyd-Warshall shortest path algorithm.
--
-- See Wikipedia article for details: https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
--
--------------------------------------------------------------------------------
module Hiraffe.ShortestPath.FloydWarshall
  ( allPairsShortestPaths
  , allPairsShortestPathsWithNext
  , allPairsShortestPathsWith
  , extractShortestPath

  , WithNext(..)
  ) where

import qualified Data.Array as Array
import           Data.Coerce
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           HGeometry.Unbounded
import           Hiraffe.Graph.Class

-------------------------------------------------------------------------------

-- |
-- All pair shortest paths. See 'allPiarsShortestPathsWith' for more info.
--
-- running time: \(O(n^3)\), where \(n\) is the number of vertices of the graph.
allPairsShortestPaths     :: forall graph dist.
                             ( HasVertices' graph, VertexIx graph ~ Int, Num dist, Ord dist)
                          => (VertexIx graph -> VertexIx graph -> Top dist)
                             -- ^ edge/dart lengths
                          -> graph
                          -> Array.Array (VertexIx graph, VertexIx graph) (Top dist)
allPairsShortestPaths len = coerce . allPairsShortestPathsWith len'
  where
    len' :: VertexIx graph -> VertexIx graph -> Top (Sum dist)
    len' = coerce len
    -- main idea is to use dynamic programming. See dist'


-- | All pair shortest paths. Annotates every vertex with its successor vertex along the
-- path. See 'allPiarsShortestPathsWith' for more info.
--
-- running time: \(O(n^3)\), where \(n\) is the number of vertices of the graph.
allPairsShortestPathsWithNext     :: forall graph dist.
                                     ( HasVertices' graph, VertexIx graph ~ Int
                                     , Num dist, Ord dist
                                     )
                                  => (VertexIx graph -> VertexIx graph -> Top dist)
                                  -- ^ edge/dart lengths
                                  -> graph
                                  -> Array.Array (VertexIx graph, VertexIx graph)
                                                 (Top (WithNext (VertexIx graph) dist))
allPairsShortestPathsWithNext len = coerce . allPairsShortestPathsWith len'
  where
    len' u v = WithNext v . Sum <$> len u v


-- | Extracts a shortest path from the table.
extractShortestPath           :: (Eq vertexIx, Array.Ix vertexIx)
                              => Array.Array (vertexIx, vertexIx)
                                             (Top (WithNext vertexIx dist))
                              -> vertexIx -- ^ source vertex
                              -> vertexIx -- ^ target vertex
                              -> Top (dist, NonEmpty.NonEmpty vertexIx)
extractShortestPath table s t = do WithNext v dist <- table Array.! (s,t)
                                   path            <- extractPath v
                                   pure (dist , if s /= v then s NonEmpty.<| path else path)
  where
    extractPath s'
      | s' == t   = pure $ NonEmpty.singleton s'
      | otherwise = do WithNext v _ <- table Array.! (s',t)
                       path         <- extractPath v
                       pure (s' NonEmpty.<| path)

-- | Keep track of the next vertex along the path.
data WithNext vertex dist = WithNext !vertex !dist
                           deriving (Show,Read)

instance Eq dist => Eq (WithNext vertex dist) where
  (WithNext _ d) == (WithNext _ d') = d == d'

instance Ord dist => Ord (WithNext vertex dist) where
  (WithNext _ d) `compare` (WithNext _ d') = d `compare` d'

instance Semigroup dist => Semigroup (WithNext vertex dist) where
  (WithNext v d) <> (WithNext _ d') = WithNext v (d <> d')


-- |
-- All pair shortest paths.
--
-- Note that essentially all information in the graph is actually encoded in the
-- given distance function; i.e. the length function may be called for any pair of
-- vertices; even if they are not connected by an edge.
--
-- running time: \(O(n^3)\), where \(n\) is the number of vertices of the graph.
allPairsShortestPathsWith        :: forall graph dist.
                                    ( HasVertices' graph
                                    , VertexIx graph ~ Int
                                    , Semigroup dist, Ord dist
                                    )
                                 => (VertexIx graph -> VertexIx graph -> Top dist)
                                    -- ^ edge/dart lengths
                                 -> graph
                                 -> Array.Array (VertexIx graph, VertexIx graph) (Top dist)
allPairsShortestPathsWith len gr = tabulate ((0,0),(n,n)) $ \(i,j) -> dist (i,j,n)
  where
    -- main idea is to use dynamic programming. See dist'
    n = numVertices gr - 1

    -- the table with distances
    dists :: Array.Array (Int,Int,Int) (Top dist)
    dists = tabulate ((0,0,(-1)), (n,n,n)) dist'

    -- | a memoized version of dist'
    dist ix = dists Array.! ix

    -- | dist (i,j,k) distance/length of the shortest path from i to j using only the
    -- first k vertices.
    dist' = \case
      (i,j,-1) -> len i j -- we cannot use intermediate edges, so just the length of the
                    -- edge from i to j, if such an edge exists.
      (i,j,k)  -> dist (i,j,pred k) `min` (liftA2 (<>) (dist (i,k,pred k))
                                                       (dist (k,j,pred k))
                                          )
        --- either we already have the minimum distance between i and j, or the
        -- shortest path goes through vertex k. So we need the path from i to k (using
        -- only intermediate vertices up to k-1), and from k to j (usign only vertices up
        -- to k-1).






-- | Create a table
tabulate       :: Array.Ix i => (i,i) -> (i -> a) -> Array.Array i a
tabulate rng f = Array.listArray rng (map f $ Array.range rng)
