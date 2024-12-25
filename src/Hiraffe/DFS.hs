--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.DFS
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of depth first search.
--
--------------------------------------------------------------------------------
module Hiraffe.DFS
  ( dfs
  , dff
  ) where

import           Control.Lens
import           Control.Monad.ST (ST, runST)
import           Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed.Mutable as UMV
import           HGeometry.Sequence.KV
import           HGeometry.Trie
import           Hiraffe.Graph
import           Witherable

--------------------------------------------------------------------------------

-- | Depth first Search (DFS) to discover all components in the graph.
--
-- pre: - the fromEnum instance of a vertex maps the vertices to the range [0,..,V-1]
--
-- \(O(V+E)\)
dff   :: (Graph_ graph, Enum (VertexIx graph), Ord (EdgeIx graph))
      => graph -> [TrieF (KV []) (EdgeIx graph) (VertexIx graph)]
dff g = runST $ do
          bv <- intitialize (numVertices g)
          flip witherM (g^..vertices.asIndex) $ \u ->
            dfsFrom g u bv

-- | DFS, from a given vertex.
--
-- pre: - the vertex actually occurs in the graph!
--      - the fromEnum instance of a vertex maps the vertices to the range [0,..,V-1]
--
-- \(O(V+E)\)
dfs         :: (Graph_ graph, Enum (VertexIx graph), Ord (EdgeIx graph))
            => graph -> VertexIx graph -> TrieF (KV []) (EdgeIx graph) (VertexIx graph)
dfs g start = runST $ do
                bv     <- intitialize (numVertices g)
                -- start will be unvisited, as per precondition, thus the fromJust is safe
                fromJust <$> dfsFrom g start bv

-- | Runs the DFS from a given start node with the given vertex
dfsFrom            :: (Graph_ graph, Enum (VertexIx graph), Ord (EdgeIx graph))
                   => graph
                   -> VertexIx graph
                   -> UMV.MVector s Bool
                   -> ST s (Maybe (TrieF (KV []) (EdgeIx graph) (VertexIx graph)))
dfsFrom g start bv = go start
  where
    go u = visited u >>= \case
      True  -> pure Nothing
      False -> do
                  visit u
                  Just . Node u . KV <$> witherM go' (g^..neighboursOfByEdge u.asIndex)

    go' (e,v) = fmap (e,) <$> go v

    visit   u = UMV.write bv (fromEnum u) True
    visited u = UMV.read  bv (fromEnum u)

-- | Initialize the vector with visited marks
intitialize   :: Int -> ST s (UMV.MVector s Bool)
intitialize n = UMV.replicate n False -- bit vector of marks


-- TODO: - I should get rid of the Enum instance, and construct this mapping myself.
--       - produce edge-labeled trees indicating which edge we are actually taking.
