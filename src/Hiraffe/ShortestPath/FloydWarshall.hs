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
  , allPairsShortestPathsWith
  ) where

import qualified Data.Array as Array
import           Data.Coerce
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
allPairsShortestPaths len = coerce . allPairsShortestPaths len'
  where
    len' :: VertexIx graph -> VertexIx graph -> Top (Sum dist)
    len' = coerce len
    -- main idea is to use dynamic programming. See dist'

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

-- -- | \( O(n^3) \)
-- floydWarshall :: (Unbox a, Fractional a, Ord a) => Int -> MVector s (a, Int) -> ST s ()
-- floydWarshall n graph = do
--     let nSq = V.length graph
--     when (n*n /= nSq) $ error "Bad bounds"
--     forM_ [0 .. n-1] $ \k ->
--       forM_ [0 .. n-1] $ \i ->
--         forM_ [0 .. n-1] $ \j -> do
--           (distIJ, _) <- access (i,j)
--           (distIK, pathIK) <- access (i,k)
--           (distKJ, _) <- access (k,j)
--           let indirectDist = distIK + distKJ
--           when (distIJ > indirectDist+indirectDist*eps && distIJ > distIK && distIJ > distKJ) $
--             put (i,j) (indirectDist, pathIK)
--   where
--     access idx = V.unsafeRead graph (mkIndex n idx)
--     put idx e = V.unsafeWrite graph (mkIndex n idx) e
--     eps = 1e-10 -- When two paths are nearly the same length, pick the one with the fewest segments.

-- -- | Compute the index of an element in a given range.
-- mkIndex :: Num a => a -> (a, a) -> a
-- mkIndex n (i,j) = i*n+j

-- -- | Construct a weighted graph from \(n\) vertices, a max bound, and a list of weighted edges.
-- mkGraph :: (Unbox a, Num a) => Int -> a -> [(Int,Int,a)] -> ST s (MVector s (a, Int))
-- mkGraph n maxValue edges = do
--   graph <- V.replicate (n*n) (maxValue, maxBound)
--   forM_ [0..n-1] $ \v -> do
--     unsafeWrite graph (mkIndex n (v,v)) (0, v)
--   forM_ edges $ \(i,j,cost) -> do
--     unsafeWrite graph (mkIndex n (i,j)) (cost, j)
--     unsafeWrite graph (mkIndex n (j,i)) (cost, i)
--   return graph
