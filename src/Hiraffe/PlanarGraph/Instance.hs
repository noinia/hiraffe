{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Instance
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The instances of the various classes for our planar grpah implementation.
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Instance
  (
  ) where

import           Control.Lens
import           Data.Functor.Apply (Apply)
import           Data.Semigroup.Traversable
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as V
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Class
import           Hiraffe.PlanarGraph.Core (PlanarGraph, VertexIdIn, FaceIdIn)
import qualified Hiraffe.PlanarGraph.Core as Core
import qualified Hiraffe.PlanarGraph.Dart as Dart
import qualified Hiraffe.PlanarGraph.Dual as Dual
import qualified Hiraffe.PlanarGraph.IO as IO
import           Hiraffe.PlanarGraph.World

--------------------------------------------------------------------------------

instance HasVertices' (PlanarGraph s w v e f) where
  type Vertex   (PlanarGraph s w v e f) = v
  type VertexIx (PlanarGraph s w v e f) = VertexIdIn w s

  vertexAt v = Core.dataOf v
  numVertices = Core.numVertices

instance HasVertices (PlanarGraph s w v e f) (PlanarGraph s w v' e f) where
  vertices = conjoined traverse' (itraverseV . indexed)
    where
      traverse' :: Apply g
                => (v -> g v') -> PlanarGraph s w v e f -> g (PlanarGraph s w v' e f)
      traverse' = Core.vertexData.traversed1
      itraverseV :: Apply g
                 => (VertexIdIn w s -> v -> g v')
                 -> PlanarGraph s w v e f -> g (PlanarGraph s w v' e f)
      itraverseV = Core.traverseVertices

----------------------------------------

instance HasDarts' (PlanarGraph s w v e f) where
  type Dart   (PlanarGraph s w v e f) = e
  type DartIx (PlanarGraph s w v e f) = Dart.Dart s
  dartAt d = Core.dataOf d
  numDarts = Core.numDarts

instance HasDarts (PlanarGraph s w v e f) (PlanarGraph s w v e' f) where
  darts = conjoined (Core.dartData.traversed1) (Core.traverseDarts . indexed)

----------------------------------------

instance HasEdges' (PlanarGraph s w v e f) where
  type Edge   (PlanarGraph s w v e f) = e
  type EdgeIx (PlanarGraph s w v e f) = Dart.Dart s
  edgeAt d = edgeAtLens d
  numEdges = Core.numEdges

-- | Edge at lens, note that it actually modifies *both* the data associated with both the
-- positive and the negative occurance of the given dart.
edgeAtLens   :: Dart.Dart s -> IndexedLens' (Dart.Dart s) (PlanarGraph s w v e f) e
edgeAtLens d = ilens (\g -> (d, get'' g)) set''
  where
    d' = Dart.asPositive d
    get'' g   = g^?!dartAt d'
    set'' g e = g&dartAt d' .~ e
                 &dartAt d  .~ e

instance HasEdges (PlanarGraph s w v e f) (PlanarGraph s w v e' f) where
  edges = itraverse'.indexed


-- mapDarts   :: (Dart.Dart s -> e -> e') -> PlanarGraph s w v e f -> PlanarGraph s w v e' f
-- mapDarts f = runIdentity . Core.traverseDarts (\i e -> Identity $ f i e)

sequenceDarts    :: Apply g => PlanarGraph s w v (g e) f -> g (PlanarGraph s w v e f)
sequenceDarts pg = pg&Core.dartData %%~ sequence1

-- TODO make sure that this somewhat finicky use of laziness works as intended!
withEdges    :: (Dart.Dart s -> e -> e') -> NonEmptyVector e -> NonEmptyVector e'
withEdges f v = let out = V.imap (\i e -> let d = toEnum i
                                          in if Dart.isPositive d
                                             then f d e
                                             else out V.! (fromEnum . Dart.twin $ d)
                                 ) v in out

itraverse'      :: Apply g
                => (Dart.Dart s -> e -> g e')
                -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
itraverse' f pg = sequenceDarts $ pg&Core.dartData %~ withEdges f

----------------------------------------

instance HasFaces' (PlanarGraph s w v e f) where
  type Face   (PlanarGraph s w v e f) = f
  type FaceIx (PlanarGraph s w v e f) = FaceIdIn w s
  faceAt fi = Core.dataOf fi
  numFaces = Core.numFaces

instance HasFaces (PlanarGraph s w v e f) (PlanarGraph s w v e f') where
  faces = conjoined (Core.faceData.traversed1) (Core.traverseFaces.indexed)

--------------------------------------------------------------------------------

instance DirGraph_ (PlanarGraph s w v e f) where
  type DirGraphFromAdjListExtraConstraints (PlanarGraph s w v e f) = (f ~ ())
  dirGraphFromAdjacencyLists = IO.fromAdjacencyLists

  endPoints = flip Core.endPoints

  outNeighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) v
      asFold  = folding  $ \g -> (\v ->     g^?! vertexAt v)  <$> Core.neighboursOf u g
      asIFold = ifolding $ \g -> (\v -> (v, g^?! vertexAt v)) <$> Core.neighboursOf u g
  {-# INLINE outNeighboursOf #-}

  outgoingDartsOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) e
      asFold  = folding  $ \g -> (\d ->     g^?! edgeAt d)  <$> Core.outgoingEdges u g
      asIFold = ifolding $ \g -> (\d -> (d, g^?! edgeAt d)) <$> Core.outgoingEdges u g
  {-# INLINE outgoingDartsOf#-}

  twinDartOf d = twinOf d . to Just

instance BidirGraph_ (PlanarGraph s w v e f) where
  twinOf d = to $ const (Dart.twin d)
  getPositiveDart _ = id

instance Graph_ (PlanarGraph s w v e f) where
  type GraphFromAdjListExtraConstraints (PlanarGraph s w v e f) = (f ~ ())

  -- | The vertices are expected to have their adjacencies in CCW order.
  fromAdjacencyLists = IO.fromAdjacencyLists

  neighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) v
      asFold  = folding  $ \g -> (\v ->     g^?! vertexAt v)  <$> Core.neighboursOf u g
      asIFold = ifolding $ \g -> (\v -> (v, g^?! vertexAt v)) <$> Core.neighboursOf u g
  {-# INLINE neighboursOf #-}

  incidentEdgesOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) e
      asFold  = folding  $ \g -> (\d ->     g^?! edgeAt d)  <$> Core.outgoingEdges u g
      asIFold = ifolding $ \g -> (\d -> (d, g^?! edgeAt d)) <$> Core.outgoingEdges u g
  {-# INLINE incidentEdgesOf #-}


instance PlanarGraph_ (PlanarGraph s w v e f) where
  type DualGraphOf (PlanarGraph s w v e f) = PlanarGraph s (DualOf w) f e v

  dualGraph = view Core.dual

  leftFace  = Dual.leftFace
  rightFace = Dual.rightFace

  nextEdge = Dual.nextEdge
  prevEdge = Dual.prevEdge

  boundaryDart = Dual.boundaryDart
  boundary = Dual.boundary
