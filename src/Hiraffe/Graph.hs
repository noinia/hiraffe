{-# LANGUAGE DefaultSignatures #-}
module Hiraffe.Graph
  ( HasVertices(..), HasVertices'(..)
  , HasEdges(..), HasEdges'(..)

  , Graph_(..)
  ) where

import Control.Lens
import Data.Kind (Type)

--------------------------------------------------------------------------------


class HasVertices' graph where
  type Vertex   graph :: Type
  type VertexIx graph :: Type

  -- -- | Traversal of all vertices in the graph, non type changing.
  -- vertices' :: IndexedTraversal' (VertexIx graph) graph (Vertex graph)
  -- default vertices'
  --   :: HasVertices graph graph => IndexedTraversal' (VertexIx graph) graph (Vertex graph)
  -- vertices' = vertices

  -- | Accessor to a given vertex.
  vertexAt :: VertexIx graph -> IndexedTraversal' (VertexIx graph) graph (Vertex graph)

  {-# MINIMAL vertexAt #-}

class HasVertices' graph => HasVertices graph graph' where
  -- | Traversal of all vertices in the graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')

--------------------------------------------------------------------------------

class HasEdges' graph where
  type Edge   graph :: Type
  type EdgeIx graph :: Type

  -- | Traversal of all edges in the graph, non type changing
  -- edges' :: IndexedTraversal' (EdgeIx graph) graph (Edge graph)
  -- default edges'
  --   :: HasEdges graph graph => IndexedTraversal' (EdgeIx graph) graph (Edge graph)
  -- edges' = edges

  edgeAt :: EdgeIx graph  -> IndexedTraversal' (EdgeIx graph) graph (Edge graph)


class HasEdges' graph => HasEdges graph graph' where
  -- | Traversal of all edges in the graph
  edges :: IndexedTraversal (EdgeIx graph) graph graph' (Edge graph) (Edge graph')


--------------------------------------


--------------------------------------

class ( HasVertices graph graph
      , HasEdges graph graph
      ) => Graph_ graph where

  -- | All neighbours of a given vertex
  neighboursOf :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)

  -- | All edges incident to a given vertex
  incidentEdges :: VertexIx graph -> IndexedFold (EdgeIx graph) graph (Edge graph)

  -- | Number of vertices in the graph.
  --
  -- running time: O(1)
  numVertices :: graph -> Int
  numVertices = lengthOf vertices

  -- | Number of edges in the graph.
  --
  -- running time: O(1)
  numEdges :: graph -> Int
  numEdges = lengthOf edges

  {-# MINIMAL neighboursOf, incidentEdges #-}
