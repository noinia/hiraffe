{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.Graph.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines various relevant graph classes
--
--------------------------------------------------------------------------------
module Hiraffe.Graph.Class
  ( HasVertices(..), HasVertices'(..)
  , HasEdges(..), HasEdges'(..)
  , HasFaces(..), HasFaces'(..)

  , Graph_(..)
  , DirGraph_
  , PlanarGraph_ -- (..)
  ) where

import Control.Lens
import Data.Kind (Type)

--------------------------------------------------------------------------------

-- | A class representing types that have vertices.
class HasVertices' graph where
  {-# MINIMAL vertexAt #-}

  -- | Vertices of the graph are of this type
  type Vertex   graph :: Type
  -- | Vertices are indexed by elements of type 'VertexIx'
  type VertexIx graph :: Type

  -- | Accessor to a given vertex.
  vertexAt :: VertexIx graph -> IndexedTraversal' (VertexIx graph) graph (Vertex graph)

  -- | Number of vertices in the graph.
  --
  -- running time: O(1)
  numVertices :: graph -> Int
  default numVertices :: HasVertices graph graph => graph -> Int
  numVertices = lengthOf vertices

-- | Class that expresses that we have a type changing traversal of all vertices.
class HasVertices' graph => HasVertices graph graph' where
  -- | Traversal of all vertices in the graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')



--------------------------------------------------------------------------------

-- | A class for things that have edges
class HasEdges' graph where
  -- | The edges of the graph are of this type
  type Edge   graph :: Type
  -- | The Edges are indexed by something of type EdgeIx
  type EdgeIx graph :: Type

  -- | Indexed traversal of a given edge.
  edgeAt :: EdgeIx graph  -> IndexedTraversal' (EdgeIx graph) graph (Edge graph)

  -- | Number of edges in the graph.
  --
  -- running time: O(1)
  numEdges :: graph -> Int
  default numEdges :: HasEdges graph graph => graph -> Int
  numEdges = lengthOf edges

-- | Class for types that have a type changing traversal of the edges
class HasEdges' graph => HasEdges graph graph' where
  -- | Traversal of all edges in the graph
  edges :: IndexedTraversal (EdgeIx graph) graph graph' (Edge graph) (Edge graph')

--------------------------------------

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


--------------------------------------


-- | A class representing directed graphs
class ( HasVertices graph graph
      , HasEdges graph graph
      ) => DirGraph_ graph where


-- | A graph representing undirected graphs
class ( HasVertices graph graph
      , HasEdges graph graph
      ) => Graph_ graph where

  -- | Build a graph from its adjacency lists.
  --
  -- If the, in the list of neighbours of vertex u we see a vertex v
  -- that itself does not appear in the adjacencylist, we may drop
  -- it. In other words if u has a neighbour v, then v better have a
  -- specification of its neighbours somewhere.
  fromAdjacencyLists :: (Foldable f, Foldable g
                        , v ~ Vertex graph
                        , e ~ Edge graph
                        ) => f (v, g (v, e)) -> graph

  -- | All neighbours of a given vertex
  --
  neighboursOf :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)

  -- | All edges incident to a given vertex
  --
  incidentEdges :: VertexIx graph -> IndexedFold (EdgeIx graph) graph (Edge graph)


  {-# MINIMAL fromAdjacencyLists, neighboursOf, incidentEdges #-}

-- | A class representing planar graphs
class ( Graph_   graph
      , HasFaces graph graph
      ) => PlanarGraph_ graph where











--------------------------------------------------------------------------------


-- -- | A 'Neighbours' is an indexed fold of vertices
-- newtype Neighbours graph = Neighbours (IndexedFold (VertexIx graph) graph (Vertex graph))

-- class HasVertices' graph => HasAdjacencies graph where
--   -- | The neighbours of all vertices in the graph
--   adjacencyLists :: IndexedFold (VertexIx graph) graph (Neighbours graph)
--   -- adjacencyLists = vertices . ito neighboursOf

--   -- | The neighbours of a particular vertex u
--   neighboursOf   :: VertexIx graph -> Neighbours graph
