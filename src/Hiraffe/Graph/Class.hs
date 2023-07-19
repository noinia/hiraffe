{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , HasDarts(..), HasDarts'(..)
  , HasFaces(..), HasFaces'(..)

  , Graph_(..)
  , DirGraph_
  , PlanarGraph_ -- (..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Graph as Containers
import           Data.Kind (Type)

--------------------------------------------------------------------------------
-- * Vertices

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
-- * Darts / Directed edges

-- | Class for types that have darts; a dart is a directed
-- edge.
class HasDarts' graph where
  -- | Type to index a dart with
  type DartIx graph :: Type
  -- | The data of a dart
  type Dart   graph :: Type

  -- | Indexed traversal of a given dart.
  dartAt :: DartIx graph  -> IndexedTraversal' (DartIx graph) graph (Dart graph)

  -- | Number of darts in the graph.
  --
  -- running time: O(1)
  numDarts :: graph -> Int
  default numDarts :: HasDarts graph graph => graph -> Int
  numDarts = lengthOf darts

-- | Class for types that have a type changing traversal of all darts
class HasDarts' graph => HasDarts graph graph' where
  -- | Traversal of all darts in the graph.
  darts :: IndexedTraversal (DartIx graph) graph graph' (Dart graph) (Dart graph')

--------------------------------------------------------------------------------
-- * Edges

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
  fromAdjacencyLists :: ( Foldable f, Foldable h
                        , vi ~ VertexIx graph
                        , v ~ Vertex graph
                        , e ~ Edge graph
                        ) => f (vi, v, h (vi, e)) -> graph

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
-- Instances for Data.Graph

instance HasVertices' Containers.Graph where
  type Vertex Containers.Graph = ()
  type VertexIx Containers.Graph = Containers.Vertex
  vertexAt u = iix u <. united
  {-# INLINE vertexAt #-}
  numVertices = F.length
  {-# INLINE numVertices #-}

instance HasVertices Containers.Graph Containers.Graph where
  vertices = itraversed <. lens (const ()) (\xs _ -> xs)
  {-# INLINE vertices #-}

instance HasDarts' Containers.Graph where
  type Dart Containers.Graph = ()
  type DartIx Containers.Graph  = (Containers.Vertex, Containers.Vertex)
  -- | Running time of traversing an edge (u,v): O(degree(u))
  dartAt (u,v) = iix u <.> neighs
    where
      neighs :: IndexedTraversal' Containers.Vertex [Containers.Vertex] ()
      neighs = traversed . filtered (== v) .> selfIndex <. united
  {-# INLINE dartAt #-}

instance HasDarts Containers.Graph Containers.Graph where
  darts = itraversed <.> neighs
    where
      neighs :: IndexedTraversal' a [a] ()
      neighs = traversed .> selfIndex <. united
  {-# INLINE darts #-}

instance HasEdges' Containers.Graph where
  type Edge Containers.Graph = Dart Containers.Graph
  type EdgeIx Containers.Graph  = DartIx Containers.Graph
  -- | Running time of traversing an edge (u,v): O(degree(u))
  edgeAt = dartAt
  {-# INLINE edgeAt #-}

instance HasEdges Containers.Graph Containers.Graph where
  edges = darts . ifiltered (\(u,v) _ -> u <= v)
  {-# INLINE edges #-}

instance Graph_ Containers.Graph where
  -- | pre: vertex Id's are in the range 0..n
  fromAdjacencyLists ajs = Containers.buildG (0,n) ds
    where
      ds = concatMap (\(u, _, neighs) -> (\(v,_) -> (u,v)) <$> F.toList neighs
                     ) $ F.toList ajs
      n = F.foldl' (\a (u,v) -> a `max` u `max` v) 0 ds
  {-# INLINE fromAdjacencyLists #-}

      -- Containers.graphFromEdges
  neighboursOf u = iix u .> traverse .> selfIndex <. united
  {-# INLINE neighboursOf #-}
  incidentEdges u = reindexed (u,) (neighboursOf u)
  {-# INLINE incidentEdges #-}
