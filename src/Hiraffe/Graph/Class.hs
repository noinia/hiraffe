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
  , DirGraph_(..)
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

-- pEFe              :: forall graph p v vi f e.
--                      ( Indexable vi p, Contravariant f, Applicative f
--                      )
--                    => graph -> vi -> p v (f v) -> p e (f e)
-- pEFe graph u pVFv = dimap eToV fVToFe pVFv
--   where
--     eiToV = undefined -- snd . endPoints graph
--     eToV :: e -> v
--     eToV = undefined -- endPoints graph e
--     fVToFe :: f v -> f e
--     fVToFe = undefined

-- theFold' =       theFold            :: forall p v vi ei f e.
--                             ( v ~ Vertex graph, vi ~ VertexIx graph
--                             , ei ~ EdgeIx graph, e ~ Edge graph
--                             , Indexable ei p
--                             , Indexable vi p, Contravariant f, Applicative f
--                             )
--                          => p v (f v) -> graph -> f graph
--       theFold pVFv graph = fGraph
--         where
--           fGraph :: f graph
--           fGraph = outgoingEdgesOf pEFe graph
--           pEFe :: p e (f e)
--           pEFe = dimap vToE fVToFe pVFv
--             where
--               vToE :: v ->
--               vToE = undefined
--               fVToFe = undefined

-- test ::

-- | A class representing directed graphs
class ( HasVertices graph graph
      , HasEdges graph graph
      ) => DirGraph_ graph where

  -- | Get the endpoints (origin, destination) of an edge
  endPoints :: graph -> EdgeIx graph -> (VertexIx graph, VertexIx graph)


 -- IndexedFold i s a = forall p f. (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

  -- | All outgoing neighbours of a given vertex
  --
  outNeighboursOf   :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)
  outNeighboursOf u = theFold
    where
      theFold            :: forall p f.
                            ( Indexable (VertexIx graph) p
                            , Contravariant f, Applicative f
                            )
                         => p (Vertex graph) (f (Vertex graph))
                         -> graph
                         -> f graph
      theFold pVFv graph = (outgoingEdgesOf u . asIndex) pEFe graph
        where
          pEFe    :: EdgeIx graph -> f (EdgeIx graph)
          pEFe ei = fE
            where
              vi = eiToVi ei
              fE = contramap eToV $ indexed pVFv vi (graph^?!vertexAt vi)

              eToV    :: EdgeIx graph -> Vertex graph
              eToV ej = graph ^?! vertexAt (eiToVi ej)

          -- pEFe :: Indexed (EdgeIx graph) (EdgeIx graph) (f (EdgeIx graph))
          -- pEFe = Indexed go

          -- go      :: EdgeIx graph -> EdgeIx graph -> f (EdgeIx graph)
          -- go ei _ = fE
          --   where
          --     vi = eiToVi ei
          --     fE = contramap eToV $ indexed pVFv vi (graph^?!vertexAt vi)

          --     eToV    :: EdgeIx graph -> Vertex graph
          --     eToV ej = graph ^?! vertexAt (eiToVi ej)

          -- get the other endpoints of the outgoing edge
          eiToVi :: EdgeIx graph -> VertexIx graph
          eiToVi = snd . endPoints graph


                --                  dimap eiToV fVToFe pVFv

                -- undefined

                --

                -- undefined


          --

          -- eiToV :: EdgeIx graph -> VertexIx graph
          -- eiToV = undefined



          -- fVToFe :: f (Vertex graph) -> f (EdgeIx graph)
          -- fVToFe = undefined

    -- outgoingEdgesOf u pAfA

  -- | All edges incident to a given vertex
  --
  outgoingEdgesOf :: VertexIx graph -> IndexedFold (EdgeIx graph) graph (Edge graph)


  -- -- | All incoming neighbours of a given vertex
  -- --
  -- inNeighboursOf :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)

  -- -- | All edges incident to a given vertex
  -- --
  -- incomingEdgesOf :: VertexIx graph -> IndexedFold (EdgeIx graph) graph (Edge graph)





-- | A graph representing undirected graphs. Note that every undirected graph is also a
-- directed graph.
class ( HasVertices graph graph
      , HasEdges graph graph
      , DirGraph_ graph
      ) => Graph_ graph where

  -- | Build a graph from its adjacency lists.
  --
  -- If the, in the list of neighbours of vertex u we see a vertex v
  -- that itself does not appear in the adjacencylist, we may drop
  -- it. In other words if u has a neighbour v, then v better have a
  -- specification of its neighbours somewhere.
  fromAdjacencyLists :: ( Foldable f, Functor f, Foldable h, Functor h
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

instance DirGraph_ Containers.Graph


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
