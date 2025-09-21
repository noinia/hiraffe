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
  , HasOuterFace(..)
  , HasInnerComponent(..)
  ) where


import Control.Lens
import Data.Vector.NonEmpty (NonEmptyVector)
import HGeometry.Lens.Util
import Hiraffe.Graph.Class
import Hiraffe.PlanarGraph.World
import Data.Functor.Apply

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
            , boundaryDartsFrom
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

  -- | Given some dart d, compute the darts along the face containing d
  -- (starting with d).  I.e. we iterate 'nextDartOf' until we are back
  -- at the starting dart.
  --
  -- Note that
  -- - if the dart is on the outerBoundary of a face, this reports the darts in CCW order
  -- - if the dart is on an an inner component, this means we report the darts in CW order.
  --
  boundaryDartsFrom :: DartIx planarGraph
                    -> IndexedFold1 (DartIx planarGraph) planarGraph (Dart planarGraph)


-- | Class stating that some type can report the inner components of a face.
class HasInnerComponent planarGraph where
  -- | Given a faceIx fi get a Fold of the inner components of the
  -- face; i.e.  we are given one dart for every inner component.  the
  -- dart has the face with index fi to its left.
  innerComponentsAt :: FaceIx planarGraph
                    -> IndexedFold (DartIx planarGraph) planarGraph (Dart planarGraph)


-- | A class for planar graphs for which we can report the outer boundary of a face.
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





-- | A class for graphs that have an outer face (and conversely, that have inner faces).
class ( PlanarGraph_ planeGraph
      ) => HasOuterFace planeGraph where
  {-# MINIMAL outerFaceId | outerFaceDart #-}
  -- | Getter to access the outer face
  outerFace :: Eq (FaceIx planeGraph)
            => IndexedLens' (FaceIx planeGraph) planeGraph (Face planeGraph)
  outerFace = singular theLens
    where
      theLens pFaceFFace g = faceAt theOuterFaceId pFaceFFace g
        where
          theOuterFaceId = outerFaceId g

  -- | Traversal of all interior faces in the graph
  interiorFaces :: (Eq (FaceIx planeGraph))
                => IndexedTraversal' (FaceIx planeGraph) planeGraph (Face planeGraph)
  interiorFaces = theTraversal
    where
      theTraversal :: (Applicative f, Indexable (FaceIx planeGraph) p)
                   => p (Face planeGraph) (f (Face planeGraph)) -> planeGraph -> f planeGraph
      theTraversal pFaceFFace g = unwrapApplicative
                                $ (faces.ifiltered (\i _ -> i /= theOuterFaceId))
                                                   (rmap WrapApplicative pFaceFFace)
                                                   g
        where
          theOuterFaceId = outerFaceId g

  -- | gets the id of the outer face
  --
  outerFaceId    :: planeGraph -> FaceIx planeGraph
  outerFaceId ps = ps^.leftFaceOf (outerFaceDart ps).asIndex

  -- | gets a dart incident to the outer face (in particular, that has the
  -- outerface on its left)
  outerFaceDart    :: planeGraph -> DartIx planeGraph
  outerFaceDart gr = gr^.boundaryDartOf (outerFaceId gr).asIndex
