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

  , reorder
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import qualified Data.Vector.Mutable as MV
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           Data.YAML
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Permutation
import           Hiraffe.PlanarGraph.AdjRep (Face (Face), Gr (Gr), Vtx (Vtx))
import           Hiraffe.PlanarGraph.Connected.Core
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.Dart (Direction(..), Arc(..))
import           Hiraffe.PlanarGraph.Connected.Dual
import           Hiraffe.PlanarGraph.EdgeOracle
import           Hiraffe.PlanarGraph.World

--------------------------------------------------------------------------------

instance (ToYAML v, ToYAML e, ToYAML f) => ToYAML (CPlanarGraph w s v e f) where
  toYAML = toYAML . toAdjRep

instance (FromYAML v, FromYAML e, FromYAML f) => FromYAML (CPlanarGraph Primal s v e f) where
  parseYAML n = fromAdjRep @s <$> parseYAML n

instance (ToJSON v, ToJSON e, ToJSON f) => ToJSON (CPlanarGraph w s v e f) where
  toEncoding = toEncoding . toAdjRep
  toJSON     = toJSON     . toAdjRep

instance (FromJSON v, FromJSON e, FromJSON f) => FromJSON (CPlanarGraph Primal s v e f) where
  parseJSON v = fromAdjRep @s <$> parseJSON v

--------------------------------------------------------------------------------

-- | Transforms the planar graph into a format that can be easily converted
-- into JSON format. For every vertex, the adjacent vertices are given in
-- counter-clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toAdjRep   :: CPlanarGraph w s v e f -> Gr (Vtx v e) (Face f)
toAdjRep g = Gr vs fs
  where
    vs = (\(u@(VertexId ui),us) -> Vtx ui (map (mkEdge u) $ F.toList us) (g^.dataOf u))
         <$> toAdjacencyLists g
    fs = (\(f,x) -> Face (outerComponentEdge f) x) <$> (toNonEmpty $ faces g)

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
--      - there is at least one vertex, and at least one edge
--
-- running time: \(O(n)\)
fromAdjRep            :: forall s v e f. Gr (Vtx v e) (Face f) -> CPlanarGraph Primal s v e f
fromAdjRep (Gr as fs) = g&faceData   .~ reorder fs' (_unVertexId._unFaceId)
  where
    -- build the actual graph using the adjacencies
    g = buildGraph as
    -- build an edge oracle so we an look up the darts
    oracle = edgeOracle g
    -- function to lookup a given dart
    findEdge' u v = fromJust $ findDart u v oracle
    -- faces are left of oriented darts
    findFace ui vi = let d = findEdge' (VertexId ui) (VertexId vi) in leftFace d g
    fs' = fromNonEmpty . fmap (\(Face (ui,vi) f) -> (findFace ui vi, f)) $ fs


-- | Read a planar graph, given by its adjacencylists in counter clockwise order.
--
-- pre: - the id's are consecutive from 0 to n (where is the number of vertices)
--      - no self-loops and no multi-edges
--
-- running time: \(O(n)\)
buildGraph    :: forall s v e. NonEmpty (Vtx v e) -> CPlanarGraph Primal s v e ()
buildGraph = fromAdjacencyLists . fmap f
  where
    f               :: Vtx v e -> (VertexIdIn Primal s, v, NonEmpty (VertexIdIn Primal s, e))
    f (Vtx vi us x) = (VertexId vi, x, NonEmpty.fromList $ first VertexId <$> us)

-- | make sure we order the data values appropriately
reorder     :: NonEmptyVector (i, a) -> (i -> Int) -> NonEmptyVector a
reorder v f = NonEmptyV.unsafeCreate $ do
                           v' <- MV.new (NonEmptyV.length v)
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
fromAdjacencyLists      :: forall s w v e g h. (Functor g, Foldable1 g, Foldable1 h, Functor h)
                        => g (VertexIdIn w s, v, h (VertexIdIn w s, e))
                        -> CPlanarGraph w s v e ()
fromAdjacencyLists adjM = gr&dartVector .~ theDartData
                            &vertexData .~ reorder theVertexData _unVertexId
  where
    gr = planarGraph' . toCycleRep n $ perm
    n  = NonEmptyV.length theDartData

    perm' :: g (NonEmpty (DartId s, e))
    perm' = toOrbit <$> adjM

    perm  :: NonEmpty (NonEmpty (DartId s))
    perm  = fmap (fmap (^._1)) . toNonEmpty  $ perm'

    theDartData   :: NonEmptyVector (DartId s, e)
    theDartData   = fromNonEmpty . foldMap1 id $ perm' -- todo, we can use a vectorbuilder here.

    theVertexData = fromFoldable1 . fmap (\(vi,v,_) -> (vi,v)) $ adjM

    -- Build an edgeOracle, so that we can query the arcId assigned to
    -- an edge in O(1) time.
    oracle :: EdgeOracle w s (Int :+ e)
    oracle = assignArcs . buildEdgeOracle . fmap (\(u,_,adj) -> (u,adj)) $ adjM

    toOrbit (u,_,adjU) = foldMap1 (toDart u) adjU

    -- if u = v we have a self-loop, so we add both a positive and a negative dart
    toDart u (v,_) = let (a :+ e) = case findEdge u v oracle of
                                      Nothing -> error $ "edge not found? " <> show (u,v)
                                      Just a' -> a'
                     in NonEmpty.fromList $ case u `compare` v of
                          LT -> [(Dart.Dart (Arc a) Positive, e)]
                          EQ -> [(Dart.Dart (Arc a) Positive, e), (Dart.Dart (Arc a) Negative, e)]
                          GT -> [(Dart.Dart (Arc a) Negative, e)]


assignArcs   :: forall s w e. EdgeOracle w s e -> EdgeOracle w s (Int :+ e)
assignArcs o = evalState (itraverseUndirected f o) 0
  where
    f     :: (VertexIdIn w s, VertexIdIn w s) -> DartData e -> State Int (DartData (Int :+ e))
    f _ e = do i <- get ; put (i+1) ; pure ((i :+) <$> e)
