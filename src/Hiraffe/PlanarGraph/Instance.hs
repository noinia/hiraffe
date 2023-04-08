{-# OPTIONS_GHC -Wno-orphans #-}
module Hiraffe.PlanarGraph.Instance
  (

  ) where

import           Control.Lens
import           Hiraffe.Graph
import           Hiraffe.PlanarGraph.Core (PlanarGraph, VertexId, FaceId)
import qualified Hiraffe.PlanarGraph.Core as Core
import           Hiraffe.PlanarGraph.Dart (Dart)

instance HasVertices' (PlanarGraph s w v e f) where
  type Vertex   (PlanarGraph s w v e f) = v
  type VertexIx (PlanarGraph s w v e f) = (VertexId s w)

  vertexAt v = ilens (\g -> (v, g^.Core.dataOf v)) (\g x -> g&Core.dataOf v .~ x)
  numVertices = Core.numVertices

instance HasVertices (PlanarGraph s w v e f) (PlanarGraph s w v' e f) where
  vertices = conjoined traverse' (itraverse' . indexed)
    where
      traverse' :: Applicative g
                => (v -> g v') -> PlanarGraph s w v e f -> g (PlanarGraph s w v' e f)
      traverse' = Core.vertexData.traversed
      itraverse' :: Applicative g
                 => (VertexId s w -> v -> g v')
                 -> PlanarGraph s w v e f -> g (PlanarGraph s w v' e f)
      itraverse' = Core.traverseVertices


instance HasEdges' (PlanarGraph s w v e f) where
  type Edge   (PlanarGraph s w v e f) = e
  type EdgeIx (PlanarGraph s w v e f) = Dart s
  edgeAt d = ilens (\g -> (d, g^.Core.dataOf d)) (\g x -> g&Core.dataOf d .~ x)
  numEdges = Core.numEdges


instance HasEdges (PlanarGraph s w v e f) (PlanarGraph s w v e' f) where
  edges = conjoined traverse' (itraverse' . indexed)
    where
      traverse' :: Applicative g
                => (e -> g e') -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
      traverse' = Core.rawDartData.traversed
      itraverse' :: Applicative g
                 => (Dart s -> e -> g e')
                 -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
      itraverse' = Core.traverseDarts


instance HasFaces' (PlanarGraph s w v e f) where
  type Face   (PlanarGraph s w v e f) = f
  type FaceIx (PlanarGraph s w v e f) = FaceId s w
  faceAt fi = ilens (\g -> (fi, g^.Core.dataOf fi)) (\g x -> g&Core.dataOf fi .~ x)
  numFaces = Core.numFaces

instance HasFaces (PlanarGraph s w v e f) (PlanarGraph s w v e f') where
  faces = conjoined traverse' (itraverse' . indexed)
    where
      traverse' :: Applicative g
                => (f -> g f') -> PlanarGraph s w v e f -> g (PlanarGraph s w v e f')
      traverse' = Core.faceData.traversed
      itraverse' :: Applicative g
                 => (FaceId s w -> f -> g f')
                 -> PlanarGraph s w v e f -> g (PlanarGraph s w v e f')
      itraverse' = Core.traverseFaces


instance Graph_ (PlanarGraph s w v e f) where
  fromAdjacencyLists = undefined

  neighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) v
      asFold  = folding  $ \g -> (\v ->     g^?! vertexAt v)  <$> Core.neighboursOf u g
      asIFold = ifolding $ \g -> (\v -> (v, g^?! vertexAt v)) <$> Core.neighboursOf u g

  incidentEdges u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) e
      asFold  = folding  $ \g -> (\d ->     g^?! edgeAt d)  <$> Core.incomingEdges u g
      asIFold = ifolding $ \g -> (\d -> (d, g^?! edgeAt d)) <$> Core.incomingEdges u g



instance PlanarGraph_ (PlanarGraph s w v e f) where
