{-# LANGUAGE DefaultSignatures #-}
module Hiraffe.Graph
  ( HasVertices(..), HasVertices'(..)
  , HasEdges(..), HasEdges'(..)
  , HasFaces(..), HasFaces'(..)

  , Graph_(..)
  , PlanarGraph_(..)
  ) where

import           Control.Lens
import qualified Data.Array as Array
import qualified Data.Foldable as F
import qualified Data.Graph as Containers
import           Data.Kind (Type)

--------------------------------------------------------------------------------

-- | A class representing types that have vertices.
class HasVertices' graph where
  {-# MINIMAL vertexAt #-}


  type Vertex   graph :: Type
  type VertexIx graph :: Type

  -- | Accessor to a given vertex.
  vertexAt :: VertexIx graph -> IndexedTraversal' (VertexIx graph) graph (Vertex graph)

  -- | Number of vertices in the graph.
  --
  -- running time: O(1)
  numVertices :: graph -> Int
  default numVertices :: HasVertices graph graph => graph -> Int
  numVertices = lengthOf vertices

-- | Class for types that support type changing traversals of all vertices.
class HasVertices' graph => HasVertices graph graph' where
  -- | Traversal of all vertices in the graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')

--------------------------------------------------------------------------------

-- | Class for types that have edges. The edges may be directed.
class HasEdges' graph where
  type Edge   graph :: Type
  type EdgeIx graph :: Type

  -- | Traversal of all edges in the graph, non type changing
  -- edges' :: IndexedTraversal' (EdgeIx graph) graph (Edge graph)
  -- default edges'
  --   :: HasEdges graph graph => IndexedTraversal' (EdgeIx graph) graph (Edge graph)
  -- edges' = edges

  -- | Indexed traversal of a given edge.
  edgeAt :: EdgeIx graph  -> IndexedTraversal' (EdgeIx graph) graph (Edge graph)

  -- | Number of edges in the graph.
  --
  -- running time: O(1)
  numEdges :: graph -> Int
  default numEdges :: HasEdges graph graph => graph -> Int
  numEdges = lengthOf edges

-- | Class for types that have a type changing traversla of all edges
class HasEdges' graph => HasEdges graph graph' where
  -- | Traversal of all edges in the graph.
  --
  -- If the graph is directed, this lists all *directed* edges.
  edges :: IndexedTraversal (EdgeIx graph) graph graph' (Edge graph) (Edge graph')

--------------------------------------
-- * Darts

-- | Class for types that have darts; a dart is a directed
-- half-edge. Moreover, each dart has a corresponding twin dart in the
-- other direction.
class HasDarts' graph where
  type Dart   graph :: Type
  type DartIx graph :: Type

  -- | Indexed traversal of a given dart.
  dartAt :: DartIx graph  -> IndexedTraversal' (DartIx graph) graph (Dart graph)

  -- | Number of edges in the graph.
  --
  -- running time: O(1)
  numDarts :: graph -> Int
  default numDarts :: HasDarts graph graph => graph -> Int
  numDarts = lengthOf darts

-- | Class for types that have a type changing traversal of all darts
class HasDarts' graph => HasDarts graph graph' where
  -- | Traversal of all edges in the graph.
  --
  -- If the graph is directed, this lists all *directed* edges.
  darts :: IndexedTraversal (DartIx graph) graph graph' (Dart graph) (Dart graph')

--------------------------------------

class HasFaces' graph where
  type Face   graph
  type FaceIx graph

  -- | Indexed traversal of a given face.
  faceAt :: FaceIx graph -> IndexedTraversal' (FaceIx graph) graph (Face graph)

  -- | Traversal of all faces in the graph, non-type chagning
  -- faces' :: IndexedTraversal' (FaceIx graph) graph (Face graph)
  -- default faces' :: HasFaces graph graph => IndexedTraversal' (FaceIx graph) graph (Face graph)
  -- faces' = faces


  -- | The number of faces in the Planar graph
  numFaces :: graph -> Int
  default numFaces :: HasFaces graph graph => graph -> Int
  numFaces = lengthOf faces

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

-- instance Graph_ Containers.Graph where
--   fromAdjacencyLists = undefined
--   neighboursOf u = iix u .> traverse . selfIndex . united
--   incidentEdges u = undefined


-- | Class representing undirected planar graphs.
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


  --------------------------------------------------------------------------------
  -- Instances for Data.Graph

instance HasVertices' Containers.Graph where
  type Vertex Containers.Graph = ()
  type VertexIx Containers.Graph = Containers.Vertex
  vertexAt u = iix u <. united
  numVertices = F.length

instance HasVertices Containers.Graph Containers.Graph where
  vertices = itraversed <. lens (const ()) (\xs _ -> xs)

instance HasEdges' Containers.Graph where
  type Edge Containers.Graph = ()
  type EdgeIx Containers.Graph  = (Containers.Vertex, Containers.Vertex)
  -- | Running time of traversing an edge (u,v): O(degree(u))
  edgeAt (u,v) = iix u <.> neighs
    where
      neighs :: IndexedTraversal' Containers.Vertex [Containers.Vertex] ()
      neighs = traversed . filtered (== v) .> selfIndex <. united

instance HasEdges Containers.Graph Containers.Graph where
  edges = itraversed <.> neighs
    where
      neighs :: IndexedTraversal' a [a] ()
      neighs = traversed .> selfIndex <. united
