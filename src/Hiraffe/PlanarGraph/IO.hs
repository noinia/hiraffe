{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Converting from/to our Adjacency-list representation of the Planar graph
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.IO
  ( toAdjRep

  , fromAdjRep
  , buildGraph
  , fromAdjacencyLists
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson
import qualified Data.Foldable as F
import           Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.YAML
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Permutation
import           Hiraffe.PlanarGraph.AdjRep (Face (Face), Gr (Gr), Vtx (Vtx))
import           Hiraffe.PlanarGraph.Core
import           Hiraffe.PlanarGraph.Dart
import           Hiraffe.PlanarGraph.Dual
import           Hiraffe.PlanarGraph.EdgeOracle

--------------------------------------------------------------------------------

instance (ToYAML v, ToYAML e, ToYAML f) => ToYAML (PlanarGraph s w v e f) where
  toYAML = toYAML . toAdjRep

instance (FromYAML v, FromYAML e, FromYAML f) => FromYAML (PlanarGraph s Primal v e f) where
  parseYAML n = fromAdjRep @s <$> parseYAML n

instance (ToJSON v, ToJSON e, ToJSON f) => ToJSON (PlanarGraph s w v e f) where
  toEncoding = toEncoding . toAdjRep
  toJSON     = toJSON     . toAdjRep

instance (FromJSON v, FromJSON e, FromJSON f) => FromJSON (PlanarGraph s Primal v e f) where
  parseJSON v = fromAdjRep @s <$> parseJSON v

--------------------------------------------------------------------------------

-- | Transforms the planar graph into a format that can be easily converted
-- into JSON format. For every vertex, the adjacent vertices are given in
-- counter-clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toAdjRep   :: PlanarGraph s w v e f -> Gr (Vtx v e) (Face f)
toAdjRep g = Gr vs fs
  where
    vs = [ Vtx ui (map (mkEdge u) $ F.toList us) (g^.dataOf u)
         | (u@(VertexId ui),us) <- toAdjacencyLists g
         ]
    fs = [ Face (outerComponentEdge f) x
         | (f,x) <- F.toList $ faces g
         ]

    outerComponentEdge f = bimap (^.unVertexId) (^.unVertexId)
                         $ endPoints (boundaryDart f g) g

    eo = edgeOracle g

    findData u v = (\d -> g^.dataOf d) <$> findDart u v eo
    mkEdge u v@(VertexId vi) = (vi,fromJust $ findData u v)


-- | Read a planar graph, given in some adjacency list representation
-- into a planar graph. The adjacencylists should be in counter
-- clockwise order.
--
-- pre: - the id's are consecutive from 0 to n (where is the number of vertices)
--      - no self-loops and no multi-edges
--
-- running time: \(O(n)\)
fromAdjRep            :: forall s v e f. Gr (Vtx v e) (Face f) -> PlanarGraph s Primal v e f
fromAdjRep (Gr as fs) = g&faceData   .~ reorder fs' (_unVertexId._unFaceId)
  where
    -- build the actual graph using the adjacencies
    g = buildGraph as
    -- build an edge oracle so we an look up the darts
    oracle = edgeOracle g
    -- function to lookup a given dart
    findEdge' u v = fromJust $ findDart u v oracle
    -- faces are right of oriented darts
    findFace ui vi = let d = findEdge' (VertexId ui) (VertexId vi) in rightFace d g
    fs' = V.fromList [ (findFace ui vi, f) | Face (ui,vi) f <- fs ]

-- | Read a planar graph, given by its adjacencylists in counter clockwise order.
--
-- pre: - the id's are consecutive from 0 to n (where is the number of vertices)
--      - no self-loops and no multi-edges
--
-- running time: \(O(n)\)
buildGraph    :: forall s v e. [Vtx v e] -> PlanarGraph s Primal v e ()
buildGraph as = fromAdjacencyLists [ (VertexId vi, x, [(VertexId ui, e) | (ui,e) <- us])
                                   | Vtx vi us x <- as
                                   ]

-- | make sure we order the data values appropriately
reorder     :: V.Vector (i, a) -> (i -> Int) -> V.Vector a
reorder v f = V.create $ do
                           v' <- MV.new (V.length v)
                           F.forM_ v $ \(i, x) ->
                             MV.write v' (f i) x
                           pure v'

--------------------------------------------------------------------------------

-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter-clockwise order.
--
-- pre: No self-loops, and no multi-edges
--
-- running time: \(O(n)\).
fromAdjacencyLists      :: forall s w v e g h. (Functor g, Foldable g, Foldable h, Functor h)
                        => g (VertexIdIn w s, v, h (VertexIdIn w s, e))
                        -> PlanarGraph s w v e ()
fromAdjacencyLists adjM = gr&dartVector .~ theDartData
                            &vertexData .~ reorder theVertexData _unVertexId
  where
    gr = planarGraph' . toCycleRep n $ perm
    n  = V.length theDartData

    perm' :: g [(DartId s, e)]
    perm' = toOrbit <$> adjM

    perm  :: [[DartId s]]
    perm  = fmap (fmap (^._1)) . F.toList $ perm'

    theDartData   = V.fromList . foldMap id $ perm' -- todo, we can use a vectorbuilder here.

    theVertexData = fromFoldable . fmap (\(vi,v,_) -> (vi,v)) $ adjM

    -- Build an edgeOracle, so that we can query the arcId assigned to
    -- an edge in O(1) time.
    oracle :: EdgeOracle s w (Int :+ e)
    oracle = assignArcs . buildEdgeOracle . fmap (\(u,_,adj) -> (u,adj)) $ adjM

    toOrbit (u,_,adjU) = foldMap (toDart u) adjU

    -- if u = v we have a self-loop, so we add both a positive and a negative dart
    toDart u (v,_) = let (a :+ e) = case findEdge u v oracle of
                                      Nothing -> error $ "edge not found? " <> show (u,v)
                                      Just a' -> a'
                     in case u `compare` v of
                          LT -> [(Dart (Arc a) Positive, e)]
                          EQ -> [(Dart (Arc a) Positive, e), (Dart (Arc a) Negative, e)]
                          GT -> [(Dart (Arc a) Negative, e)]


assignArcs   :: forall s w e. EdgeOracle s w e -> EdgeOracle s w (Int :+ e)
assignArcs o = evalState (itraverseUndirected f o) 0
  where
    f     :: (VertexIdIn w s, VertexIdIn w s) -> DartData e -> State Int (DartData (Int :+ e))
    f _ e = do i <- get ; put (i+1) ; pure ((i :+) <$> e)
