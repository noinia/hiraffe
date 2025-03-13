--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Connected
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing connected planar graphs
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Connected
  ( -- $setup
    -- * The Planar Graph type
    CPlanarGraph
  , Core.embedding, vertexData, dartData, faceData

  , World(..)
  , DualOf

    -- * Representing edges: Arcs and Darts
  , Core.DartId, Dart.arc, Dart.direction
  , Dart.twin, Dart.isPositive, Dart.asPositive
  , Dart.Arc
  , Dart.Direction(..), Dart.rev

  -- * Building a planar graph
  , cPlanarGraph, Core.cPlanarGraph'
  , fromAdjRep
  , fromAdjacencyRep

  -- * Exporting a planar graph
  , Core.toAdjacencyLists
  , toAdjRep

  -- * Darts

  -- * Edges

  -- * Vertices
  , VertexIdIn(..), VertexId
  , Core.incidentEdges
  , Core.incomingEdges
  , Core.outgoingEdges
  , Core.nextIncidentEdge, Core.prevIncidentEdge
  , Core.nextIncidentEdgeFrom, Core.prevIncidentEdgeFrom

  -- * Faces
  , FaceIdIn(..), FaceId
  , boundary'

  -- * Dual Graph
  , Core.dual

  -- * Associated Data
  , Core.endPointDataOf

  , module Hiraffe.PlanarGraph.Class
  , module Hiraffe.Graph.Class
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Class
import           Hiraffe.PlanarGraph.Connected.Core ( CPlanarGraph
                                                    , VertexIdIn(..), VertexId
                                                    , FaceIdIn(..), FaceId
                                                    )
import           Hiraffe.PlanarGraph.Connected.Core (cPlanarGraph, vertexData, faceData, dartData)
import qualified Hiraffe.PlanarGraph.Connected.Core as Core
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.Connected.Dual
import           Hiraffe.PlanarGraph.IO
import           Hiraffe.PlanarGraph.Connected.Instance ()
import           Hiraffe.PlanarGraph.World


--------------------------------------------------------------------------------
-- $setup
-- >>> import qualified Data.Vector.NonEmpty as V
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> import Control.Lens
-- >>> import Hiraffe.PlanarGraph.Dart(Dart(Dart),Arc(Arc),Direction(..))
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     adjacencies = NonEmpty.fromList . fmap NonEmpty.fromList $
--                           [ [ (Dart aA Negative, "a-")
--                             , (Dart aC Positive, "c+")
--                             , (Dart aB Positive, "b+")
--                             , (Dart aA Positive, "a+")
--                             ]
--                           , [ (Dart aE Negative, "e-")
--                             , (Dart aB Negative, "b-")
--                             , (Dart aD Negative, "d-")
--                             , (Dart aG Positive, "g+")
--                             ]
--                           , [ (Dart aE Positive, "e+")
--                             , (Dart aD Positive, "d+")
--                             , (Dart aC Negative, "c-")
--                             ]
--                           , [ (Dart aG Negative, "g-")
--                             ]
--                           ]
--     myGraph :: CPlanarGraph Primal String String String String
--     myGraph = cPlanarGraph  adjacencies
--                  & vertexData .~ V.unsafeFromList ["u","v","w","x"]
--                  & faceData   .~ V.unsafeFromList ["f_3", "f_infty","f_1","f_2"]
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlanarGraph/Connected/testG.png)


--------------------------------------------------------------------------------
-- Testing stuff

-- testPerm :: Permutation (Dart s)
-- testPerm = let (a:b:c:d:e:g:_) = take 6 [Arc 0..]
--            in toCycleRep 12 [ [ Dart a Negative
--                               , Dart c Positive
--                               , Dart b Positive
--                               , Dart a Positive
--                               ]
--                             , [ Dart e Negative
--                               , Dart b Negative
--                               , Dart d Negative
--                               , Dart g Positive
--                               ]
--                             , [ Dart e Positive
--                               , Dart d Positive
--                               , Dart c Negative
--                               ]
--                             , [ Dart g Negative
--                               ]
--                             ]

-- data Test

-- testG :: PlanarGraph Test Primal () String ()
-- testG = cPlanarGraph [ [ (Dart aA Negative, "a-")
--                       , (Dart aC Positive, "c+")
--                       , (Dart aB Positive, "b+")
--                       , (Dart aA Positive, "a+")
--                       ]
--                     , [ (Dart aE Negative, "e-")
--                       , (Dart aB Negative, "b-")
--                       , (Dart aD Negative, "d-")
--                       , (Dart aG Positive, "g+")
--                       ]
--                     , [ (Dart aE Positive, "e+")
--                       , (Dart aD Positive, "d+")
--                       , (Dart aC Negative, "c-")
--                       ]
--                     , [ (Dart aG Negative, "g-")
--                       ]
--                     ]
--   where
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]






--------------------------------------------------------------------------------

-- -- | Indexed traversal of all darts in the planar graph.
-- darts :: forall s w v e e' f.
--          IndexedTraversal (Dart s) (CPlanarGraph w s v e f) (CPlanarGraph w s v e' f) e e'
-- darts = conjoined traverse' (itraverse' . indexed)
--     where
--       traverse' :: Applicative g
--                 => (e -> g e') -> CPlanarGraph w s v e f -> g (CPlanarGraph w s v e' f)
--       traverse' = Core.dartData.traversed
--       itraverse' :: Applicative g
--                  => (Dart s -> e -> g e')
--                  -> CPlanarGraph w s v e f -> g (CPlanarGraph w s v e' f)
--       itraverse' = Core.traverseDarts




-- | Given a connected plane graph in adjacency list format; convert it into an actual
-- PlanarGraph.
--
-- \(O(n\log n)\)
fromAdjacencyRep             :: (Ord i, Foldable1 f)
                             => proxy s -> GGraph f i v e -> CPlanarGraph Primal s v e ()
fromAdjacencyRep _ (Graph m) = (cPlanarGraph theDarts)&vertexData .~ vtxData
  where
    vtxData = (\(VertexData x _ _) -> x) <$> fromFoldable1 m
    --  a non-empty list of vertices, with for each vertex the darts in order around the vertex
    theDarts  = evalState (sequence' theDarts') (0 :+ Map.empty)
    theDarts' = toNonEmpty $ NEMap.mapWithKey toIncidentDarts m
    -- turn the outgoing edges of u into darts
    toIncidentDarts u (VertexData _ neighMap neighOrder) =
      (\v -> (toDart u v, neighMap Map.! u)) <$> toNonEmpty neighOrder
    -- create the dart corresponding to vertices u and v

    toDart u v | u <= v    =  flip Dart.Dart Dart.Positive <$> arc u v
               | otherwise =  flip Dart.Dart Dart.Negative <$> arc v u

    arc u v = gets (arcOf (u,v)) >>= \case
                Just a  -> pure a
                Nothing -> do a <- nextArc
                              modify $ insertArc (u,v) a
                              pure a

    arcOf x       = Map.lookup x . view extra
    insertArc k v = over extra $ Map.insert k v

    nextArc = do i <- gets (view core)
                 modify $ over core (+1)
                 pure $ Dart.Arc i

-- | Helper to implement fromAdjacencyRep
sequence' :: Applicative m => NonEmpty (NonEmpty (m a, b)) -> m (NonEmpty (NonEmpty (a,b)))
sequence' = traverse $ traverse (\(fa,b) -> (,b) <$> fa)
