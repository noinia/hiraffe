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
  , HasOuterBoundaryOf(..)
  -- , HasBoundary(..)
  ) where


import Control.Lens
import Data.Vector.NonEmpty (NonEmptyVector)
import HGeometry.Lens.Util
import Hiraffe.Graph.Class
import Hiraffe.PlanarGraph.World

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
  faces :: IndexedTraversal1 (FaceIx graph) graph graph' (Face graph) (Face graph')

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | A class representing planar graphs
class ( Graph_   planarGraph
      , HasFaces planarGraph planarGraph
      -- , PlanarGraph_ (DualGraphOf planarGraph)


      --   -- these conditions may be too strict
      -- , VertexIx (DualGraphOf planarGraph) ~ FaceIx planarGraph
      -- , Vertex   (DualGraphOf planarGraph) ~ Face   planarGraph
      -- , FaceIx (DualGraphOf planarGraph) ~ VertexIx planarGraph
      -- , Face   (DualGraphOf planarGraph) ~ Vertex   planarGraph

      ) => PlanarGraph_ planarGraph where

  {-# MINIMAL dualGraph, (incidentFaceOf|leftFaceOf)
            , _DualFaceIx, _DualVertexIx
            , prevDartOf, nextDartOf, boundaryDartOf
    #-}

  type DualGraphOf planarGraph
  type DualGraphOf planarGraph = planarGraph

  type WorldOf planarGraph :: World
  type WorldOf planarGraph = Primal

  -- | The dual of this graph
  dualGraph :: planarGraph -> DualGraphOf planarGraph

  -- | Convert from a VertexIx in the primal graph to a FaceIx in the
  -- dual graph.
  --
  -- (Likely this will just be the identiy function or a simple coercion)
  _DualFaceIx   :: proxy planarGraph
                -> Iso' (VertexIx planarGraph) (FaceIx (DualGraphOf planarGraph))

  -- | Convert from a FaceIx in the primal graph to a VertexIx in the
  -- dual graph.
  --
  -- (Likely this will just be the identiy function or a simple coercion)
  _DualVertexIx :: proxy planarGraph
                -> Iso' (FaceIx planarGraph) (VertexIx (DualGraphOf planarGraph))


  -- | The face to the left of the dart
  incidentFaceOf :: DartIx planarGraph
                 -> IndexedLens' (FaceIx planarGraph) planarGraph (Face planarGraph)
  incidentFaceOf = leftFaceOf

  -- | The face to the left of the dart. Alternative name for incidentFaceOf
  leftFaceOf :: DartIx planarGraph
             -> IndexedLens' (FaceIx planarGraph) planarGraph (Face planarGraph)
  leftFaceOf = incidentFaceOf

  -- | The face to the right of the dart
  rightFaceOf   :: DartIx planarGraph
                -> IndexedLens' (FaceIx planarGraph) planarGraph (Face planarGraph)
  rightFaceOf d = \l gr -> leftFaceOf (gr^.twinOf d) l gr

  -- | Get the previous edge in order along the face (so ccw for internal faces, and cw
  -- for external faces) that is to the left of this dart.
  prevDartOf   :: DartIx planarGraph
               -> IndexedLens' (DartIx planarGraph) planarGraph (Dart planarGraph)

  -- | Get the next edge in order along the face (so ccw for internal faces, and cw for
  -- external faces) that is to the left of this dart.
  nextDartOf   :: DartIx planarGraph
               -> IndexedLens' (DartIx planarGraph) planarGraph (Dart planarGraph)

  -- | Gets a dart bounding this face. I.e. a dart d such that the face lies to
  -- the left of the dart.
  boundaryDartOf :: FaceIx planarGraph
                 -> IndexedLens' (DartIx planarGraph) planarGraph (Dart planarGraph)

-- | A class for things that have a boundary.
class ( DiGraph_ planarGraph
      , HasFaces planarGraph planarGraph
      ) => HasOuterBoundaryOf planarGraph where
  {-# MINIMAL outerBoundaryDarts  #-}

  -- | The darts on the outer boundary of this face, in counter clockwise order along the
  -- boundary.
  --
  -- pre: faceIx /= outerFaceIx
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  outerBoundaryDarts :: FaceIx planarGraph -> planarGraph -> NonEmptyVector (DartIx planarGraph)

  -- | The darts on the outer boundary of the face, in counter clockwise order along the
  -- boundary.
  --
  -- pre: faceIx /= outerFaceIx
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  outerBoundaryDartsOf    :: FaceIx planarGraph
                          -> IndexedFold1 (DartIx planarGraph) planarGraph (Dart planarGraph)
  outerBoundaryDartsOf fi = ifolding1 $ \g ->
                              (\d -> g^?!dartAt d.withIndex) <$> outerBoundaryDarts fi g

  -- | The vertices on the outer boundary of the face, in counter clockwise order along the
  -- boundary.
  --
  -- pre: faceIx /= outerFaceIx
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  outerBoundaryVertices      :: FaceIx planarGraph -> planarGraph
                             -> NonEmptyVector (VertexIx planarGraph)
  outerBoundaryVertices fi g = (\d -> g^.tailOf d.asIndex) <$> outerBoundaryDarts fi g

  -- | The vertices on the outer boundary of the face, in counter clockwise order along
  -- the boundary.
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  outerBoundaryVerticesOf    :: FaceIx planarGraph
                             -> IndexedFold1 (VertexIx planarGraph) planarGraph (Vertex planarGraph)
  outerBoundaryVerticesOf fi = ifolding1 $ \g ->
                                 (\d -> g^.tailOf d.withIndex) <$> outerBoundaryDarts fi g



-- | A class for things that have a boundary.
class ( DiGraph_ planarGraph
      , HasFaces planarGraph planarGraph
      ) => HasBoundary planarGraph where
  {-# MINIMAL boundaryDarts  #-}

  -- TODO: this is underspecified; do we want only the outer boundary? Or also the inner boundaries?

  -- | The darts bounding this face. The darts are reported in order
  -- along the face. This means that for internal faces the darts are
  -- reported in *counter clockwise* order along the boundary, whereas for the
  -- outer face the darts are reported in clockwise order.
  --
  --
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  boundaryDarts :: FaceIx planarGraph -> planarGraph -> NonEmptyVector (DartIx planarGraph)

  -- | The darts are reported in order along the face. This means that for internal faces
  -- the darts are reported in *counter clockwise* order along the boundary, whereas for the
  -- outer face the darts are reported in clockwise order.
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  boundaryDartsOf    :: FaceIx planarGraph
                     -> IndexedFold1 (DartIx planarGraph) planarGraph (Dart planarGraph)
  boundaryDartsOf fi = ifolding1 $ \g ->
                                  (\d -> g^?!dartAt d.withIndex) <$> boundaryDarts fi g

  -- | The vertices bounding this face, for internal faces in counter clockwise
  -- order, for the outer face in clockwise order.
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  boundaryVertices      :: FaceIx planarGraph -> planarGraph
                        -> NonEmptyVector (VertexIx planarGraph)
  boundaryVertices fi g = (\d -> g^.tailOf d.asIndex) <$> boundaryDarts fi g

  -- | The vertices bounding this face, for internal faces in counter clockwise
  -- order, for the outer face in clockwise order.
  --
  -- running time: \(O(k)\), where \(k\) is the output size.
  boundaryVerticesOf    :: FaceIx planarGraph
                        -> IndexedFold1 (VertexIx planarGraph) planarGraph (Vertex planarGraph)
  boundaryVerticesOf fi = ifolding1 $ \g ->
                            (\d -> g^.tailOf d.withIndex) <$> boundaryDarts fi g
