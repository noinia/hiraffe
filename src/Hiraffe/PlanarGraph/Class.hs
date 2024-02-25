{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines various relevant graph classes
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Class
  ( PlanarGraph_(..)
  , HasFaces(..), HasFaces'(..)
  ) where


import           Control.Lens
import qualified Data.Vector as V
import           Hiraffe.Graph.Class

--------------------------------------------------------------------------------
-- * Faces

-- | A Class for types that have faces.
class HasFaces' graph where
  -- | The faces are of type Face graph
  type Face   graph
  -- | Faces are indexed by something of type 'FaceIx graph'
  type FaceIx graph

  -- | Indexed traversal of a given face.
  faceAt :: FaceIx graph -> IndexedTraversal' (FaceIx graph) graph (Face graph)

  -- | The number of faces in the Planar graph
  numFaces :: graph -> Int
  default numFaces :: HasFaces graph graph => graph -> Int
  numFaces = lengthOf faces

-- | Types that have a possibly type changing traversal of all faces.
class HasFaces' graph => HasFaces graph graph' where

  -- | Traversal of all faces in the graph
  faces :: IndexedTraversal (FaceIx graph) graph graph' (Face graph) (Face graph')

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | A class representing planar graphs
class ( Graph_   planarGraph
      , HasFaces planarGraph planarGraph
      -- , PlanarGraph_ (DualGraphOf planarGraph)
      ) => PlanarGraph_ planarGraph where

  {-# MINIMAL dualGraph, leftFace, rightFace, prevEdge, nextEdge, boundaryDart, boundary
    #-}

  type DualGraphOf planarGraph
  type DualGraphOf planarGraph = planarGraph

  -- | The dual of this graph
  dualGraph :: planarGraph -> DualGraphOf planarGraph

  -- | The face to the left of the dart
  leftFace     :: DartIx planarGraph -> planarGraph -> FaceIx planarGraph

  -- | The face to the right of the dart
  rightFace     :: DartIx planarGraph -> planarGraph -> FaceIx planarGraph

  -- | Get the previous edge in order along the face (so ccw for internal faces, and cw
  -- for external faces ) that is to the left of this dart.
  prevEdge   :: DartIx planarGraph -> planarGraph -> DartIx planarGraph

  -- | Get the next edge in order along the face (so ccw for internal faces, and cw for
  -- external faces ) that is to the left of this dart.
  nextEdge   :: DartIx planarGraph -> planarGraph -> DartIx planarGraph

  -- | Gets a dart bounding this face. I.e. a dart d such that the face lies to
  -- the left of the dart.
  boundaryDart :: FaceIx planarGraph -> planarGraph -> DartIx planarGraph

  -- | The darts bounding this face. The darts are reported in order
  -- along the face. This means that for internal faces the darts are
  -- reported in *counter clockwise* order along the boundary, whereas for the
  -- outer face the darts are reported in clockwise order.
  boundary :: FaceIx planarGraph -> planarGraph -> V.Vector (DartIx planarGraph)

  -- | The vertices bounding this face, for internal faces in counter clockwise
  -- order, for the outer face in clockwise order.
  boundaryVertices     :: FaceIx planarGraph -> planarGraph -> V.Vector (VertexIx planarGraph)
  boundaryVertices f g = (\d -> g^.tailOf d) <$> boundary f g

  -- | The vertices bounding this face, for internal faces in counter clockwise
  -- order, for the outer face in clockwise order.
  boundaryVerticesOf    :: FaceIx planarGraph
                        -> IndexedFold (VertexIx planarGraph) planarGraph (Vertex planarGraph)
  boundaryVerticesOf fi = ifolding $ \g ->
                            (\vi -> (vi,g^?!vertexAt vi)) <$> boundaryVertices fi g
