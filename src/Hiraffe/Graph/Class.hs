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
  ( Graph_(..)
  , ConstructableGraph_(..)
  , BidirGraph_(..)
  , DiGraph_(..)
  , ConstructableDiGraph_(..)
  , HasVertices(..), HasVertices'(..)
  , HasEdges(..), HasEdges'(..)
  , HasDarts(..), HasDarts'(..)
  ) where

import           Control.Lens
import qualified Data.Array as Array
import qualified Data.Foldable as F
import           Data.Foldable1 (Foldable1)
import           Data.Functor.Apply (Apply)
import qualified Data.Graph as Containers
import           Data.Kind (Type, Constraint)
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext

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

-- | Class that expresses that we have a non-empty type changing traversal of all vertices.
--
class HasVertices' graph => HasVertices graph graph' where
  -- | Traversal of all vertices in the graph
  vertices :: IndexedTraversal1 (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')

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

--------------------------------------

-- | A class representing non-empty directed graphs
class ( HasVertices graph graph
      , HasDarts graph graph
      ) => DiGraph_ graph where
  {-# MINIMAL (endPoints | headOf, tailOf)
            , (outNeighboursOfByDart | outgoingDartsOf)
            , twinDartOf
   #-}

  -- | Get the endpoints (origin, destination) of a dart
  endPoints     :: graph -> DartIx graph -> (VertexIx graph, VertexIx graph)
  endPoints g d = (g^.headOf d.asIndex, g^.tailOf d.asIndex)
  {-# INLINE endPoints #-}

  -- | Given a dart, produce an indexed getter to access the endpoints (u,v) of the dart.
  -- where u is the origin/tail, and v is the destination/head.
  endPointsOf   :: DartIx graph
                -> IndexedGetter (VertexIx graph, VertexIx graph)
                                 graph
                                 (Vertex graph, Vertex graph)
  endPointsOf d = ito $ \g -> let e@(u,v) = endPoints g d
                              in (e, (g^?!vertexAt u, g^?!vertexAt v))
  {-# INLINE endPointsOf #-}

  -- | All outgoing neighbours of a given vertex
  --
  outNeighboursOf   :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)
  outNeighboursOf u = reindexed snd $ outNeighboursOfByDart u


  -- | All outgoing Neighbours, indexed by the Dart towards them as well as their
  -- vertexId
  outNeighboursOfByDart   :: VertexIx graph
                          -> IndexedFold (DartIx graph, VertexIx graph) graph (Vertex graph)
  outNeighboursOfByDart u = theFold
    where
      theFold paFa graph = (outgoingDartsOf u . asIndex) otherVtx graph
        where
          otherVtx e = let v = otherVertexIx e
                       in contramap otherVertex $ indexed paFa (e,v) (graph^?!vertexAt v)
          -- otherVertexIx  :: DartIx graph -> VertexIx graph
          otherVertexIx = snd . endPoints graph
          -- otherVertex    :: DartIx graph -> Vertex graph
          otherVertex e = graph^?!vertexAt (otherVertexIx e)
  {-# INLINE outNeighboursOf #-}


  -- | All outgoing darts incident to a given vertex
  --
  outgoingDartsOf   :: VertexIx graph -> IndexedFold (DartIx graph) graph (Dart graph)
  default outgoingDartsOf :: (DartIx graph ~ (VertexIx graph, VertexIx graph))
                    => VertexIx graph -> IndexedFold (DartIx graph) graph (Dart graph)
  outgoingDartsOf u = theFold
    where
      theFold peFe graph = (outNeighboursOf u . asIndex) outDart graph
        where
          -- outDart :: VertexIx graph -> f (VertexIx graph)
          outDart v = let e = toDartIx v
                      in contramap toDart $ indexed peFe e (toDart v)

          toDartIx = (u,)
          -- toDart   :: VertexIx graph -> Dart graph
          toDart v = graph^?!dartAt (toDartIx v)
  {-# INLINE outgoingDartsOf#-}

  -- | Given two vertices u and v, get the dart representing u and v (if such a dart
  -- exists).
  dartIxFromTo         :: graph -> VertexIx graph -> VertexIx graph -> Maybe (DartIx graph)
  default dartIxFromTo :: Eq (VertexIx graph)
                       => graph -> VertexIx graph -> VertexIx graph -> Maybe (DartIx graph)
  dartIxFromTo gr u v  = preview (outgoingDartsOf u . asIndex . filtered toV) gr
    where
      toV d = gr^.headOf d.asIndex == v

  -- | The twin of this dart, if it exits
  twinDartOf :: DartIx graph -> Getter graph (Maybe (DartIx graph))

  -- | The vertex this dart is heading in to (i.e. its destination.)
  --
  headOf   :: DartIx graph -> IndexedGetter (VertexIx graph) graph (Vertex graph)
  headOf d = ito $ \g -> let vi = snd $ endPoints g d
                         in (vi, g^?!vertexAt vi)

  -- | The tail of a dart, i.e. the vertex this dart is leaving from (i.e. its origin)
  --
  tailOf   :: DartIx graph -> IndexedGetter (VertexIx graph) graph (Vertex graph)
  tailOf d = ito $ \g -> let vi = fst $ endPoints g d
                         in (vi, g^?!vertexAt vi)


-- | A class representing non-empty directed graphs
class ( DiGraph_ graph
      ) => ConstructableDiGraph_ graph where
  {-# MINIMAL diGraphFromAdjacencyLists #-}
  -- | Possible additional constraints for constructing a DirGraph
  type DiGraphFromAdjListExtraConstraints graph (h :: Type -> Type) :: Constraint
  type DiGraphFromAdjListExtraConstraints graph h = ()

  -- | Build a directed graph from its adjacency lists.
  diGraphFromAdjacencyLists :: ( Foldable1 f, Functor f, Foldable h, Functor h
                               , vi ~ VertexIx graph
                               , v ~ Vertex graph
                               , d ~ Dart graph
                               , DiGraphFromAdjListExtraConstraints graph h
                               ) => f (vi, v, h (vi, d)) -> graph

--------------------------------------------------------------------------------


-- | Types representing non-empty bidirected graphs, i.e. a directed graph, but all directed edges
-- are guaranteed to exist in both directions.
class DiGraph_ graph => BidirGraph_ graph where

  -- | The twin of this dart.
  twinOf :: DartIx graph -> Getter graph (DartIx graph)

  -- | Given An edgeIx, gets the positive dart that (together with its twin) represents
  -- this edge.
  getPositiveDart :: graph -> EdgeIx graph -> DartIx graph


  -- -- | All incoming neighbours of a given vertex
  -- --
  -- inNeighboursOf :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)

  -- -- | All edges incident to a given vertex
  -- --
  -- incomingEdgesOf :: VertexIx graph -> IndexedFold (EdgeIx graph) graph (Edge graph)


-- | A class representing non-empty undirected graphs. Note that every undirected graph is also a
-- directed graph.
class ( BidirGraph_ graph
      ) => Graph_ graph where

  -- | All neighbours of a given vertex
  --
  neighboursOf   :: VertexIx graph -> IndexedFold (VertexIx graph) graph (Vertex graph)
  neighboursOf u = reindexed snd $ neighboursOfByEdge u

  -- | All edges incident to a given vertex
  --
  incidentEdgesOf :: VertexIx graph -> IndexedFold (EdgeIx graph) graph (Edge graph)

  -- | Get the neighbours of a given vertex, indexed by the edgeIx and the vertexIx
  neighboursOfByEdge :: VertexIx graph
                     -> IndexedFold (EdgeIx graph, VertexIx graph) graph (Vertex graph)

  {-# MINIMAL neighboursOfByEdge, incidentEdgesOf #-}


class ( Graph_ graph
      ) => ConstructableGraph_ graph where
  {-# MINIMAL fromAdjacencyLists #-}


  -- | Possible additional constraints for constructing a DirGraph
  type GraphFromAdjListExtraConstraints graph (h :: Type -> Type)  :: Constraint
  type GraphFromAdjListExtraConstraints graph h = ()

  -- | Build a graph from its adjacency lists.
  --
  -- If the, in the list of neighbours of vertex u we see a vertex v
  -- that itself does not appear in the adjacencylist, we may drop
  -- it. In other words if u has a neighbour v, then v better have a
  -- specification of its neighbours somewhere.
  fromAdjacencyLists :: ( Foldable1 f, Functor f, Foldable h, Functor h
                        , vi ~ VertexIx graph
                        , v ~ Vertex graph
                        , e ~ Edge graph
                        , GraphFromAdjListExtraConstraints graph h
                        ) => f (vi, v, h (vi, e)) -> graph

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
  -- ^ Note this instance is technically unsafe, since one can construct an empty graph.
  vertices = unsafeItraversed1 <. lens (const ()) (\xs _ -> xs)
  {-# INLINE vertices #-}

-- | An unsafe Itraversed1 version for Array.
--
-- (Essentially by just unfolding the defs of traverse) and itraverse, injecting FromNonEmtpy
-- in the right places.
unsafeItraversed1 :: forall i a b. Array.Ix i =>
                     IndexedTraversal1 i (Array.Array i a) (Array.Array i b) a b
unsafeItraversed1 = conjoined traverse1' (itraverse1' . indexed)
  where
    mkArray      :: (i, i) -> NonEmpty.NonEmpty b
                 -> Array.Array i b
    mkArray bnds = Array.listArray bnds . F.toList
    traverse1'       :: Apply f => (a -> f b) -> Array.Array i a -> f (Array.Array i b)
    traverse1' f arr =
      mkArray (Array.bounds arr) <$> traverse1 f (fromList' $ Array.elems arr)
    itraverse1'       :: Apply f => (i -> a -> f b) -> Array.Array i a -> f (Array.Array i b)
    itraverse1' f arr =
      mkArray (Array.bounds arr) <$> traverse1 (uncurry' f) (fromList' $ Array.assocs arr)

    fromList' xs = case NonEmpty.nonEmpty xs of
      Nothing -> error "Hiraffe.Graph.Class.vertices on Containers.Graph: Empty graph!"
      Just ns -> ns
    uncurry' f (a, b) = f a b


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

instance DiGraph_ Containers.Graph where
  endPoints _ = id
  {-# INLINE endPoints #-}
  outNeighboursOf u = iix u .> traverse .> selfIndex <. united
  {-# INLINE outNeighboursOf #-}
  outNeighboursOfByDart u = reindexed (\v -> ((u,v),v)) $ outNeighboursOf u
  {-# INLINE outNeighboursOfByDart #-}

  -- | The twin of this dart (u,v), if it exits.
  --
  -- O(d), where d is the out degree of vertex u.
  twinDartOf (u,v) = to $ \g -> (v,u) <$ F.find (== v) (g Array.! u)

instance ConstructableDiGraph_ Containers.Graph where
  -- | pre: vertex Id's are in the range 0..n
  diGraphFromAdjacencyLists ajs = Containers.buildG (0,n) ds
    where
      ds = concatMap (\(u, _, neighs) -> (\(v,_) -> (u,v)) <$> F.toList neighs
                     ) $ F.toList ajs
      n = F.foldl' (\a (u,v) -> a `max` u `max` v) 0 ds
  {-# INLINE diGraphFromAdjacencyLists #-}

instance BidirGraph_ Containers.Graph where
  twinOf (u,v) = to $ const (v,u)
  getPositiveDart _ d@(u,v) | u <= v    = d
                            | otherwise = (v,u)
  -- we use the dart oriented from small to large as the positive one.

instance Graph_ Containers.Graph where
  -- Containers.graphFromEdges
  neighboursOf u = iix u .> traverse .> selfIndex <. united
  {-# INLINE neighboursOf #-}
  incidentEdgesOf u = reindexed (u,) (neighboursOf u)
  {-# INLINE incidentEdgesOf #-}
  neighboursOfByEdge u = reindexed (\v -> ((u,v),v)) $ neighboursOf u
  {-# INLINE neighboursOfByEdge #-}

instance ConstructableGraph_ Containers.Graph where
  -- | pre: vertex Id's are in the range 0..n
  fromAdjacencyLists ajs = Containers.buildG (0,n) ds
    where
      ds = concatMap (\(u, _, neighs) -> (\(v,_) -> (u,v)) <$> F.toList neighs
                     ) $ F.toList ajs
      n = F.foldl' (\a (u,v) -> a `max` u `max` v) 0 ds
  {-# INLINE fromAdjacencyLists #-}


--------------------------------------------------------------------------------

instance HasVertices' graph => HasVertices' (graph :+ extra) where
  type Vertex   (graph :+ extra) = Vertex graph
  type VertexIx (graph :+ extra) = VertexIx graph
  vertexAt u = core.vertexAt u
  numVertices = numVertices . view core

instance HasVertices graph graph' => HasVertices (graph :+ extra) (graph' :+ extra) where
  vertices = core.vertices

instance HasDarts' graph => HasDarts' (graph :+ extra) where
  type DartIx (graph :+ extra) = DartIx graph
  type Dart   (graph :+ extra) = Dart graph
  dartAt d = core.dartAt d
  numDarts = numDarts . view core

instance HasDarts graph graph' => HasDarts (graph :+ extra) (graph' :+ extra) where
  darts = core.darts

instance HasEdges' graph => HasEdges' (graph :+ extra) where
  type EdgeIx (graph :+ extra) = EdgeIx graph
  type Edge   (graph :+ extra) = Edge graph
  edgeAt d = core.edgeAt d
  numEdges = numEdges . view core

instance HasEdges graph graph' => HasEdges (graph :+ extra) (graph' :+ extra) where
  edges = core.edges
