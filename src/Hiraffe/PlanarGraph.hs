--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing connected planar graphs
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph
  ( -- $setup
    -- * The Planar Graph type
    PlanarGraph
  , Core.embedding -- , Core.vertexData, Core.dartData, Core.faceData, Core.rawDartData

  , World(..)
  , DualOf

    -- * Representing edges: Arcs and Darts
  , Dart(Dart), arc, direction
  , twin, isPositive, asPositive
  , Arc(..)
  , Direction(..), rev

  -- * Building a planar graph
  , Core.planarGraph, Core.planarGraph'
  , fromAdjRep

  -- * Exporting a planar graph
  , Core.toAdjacencyLists
  , toAdjRep

  -- * Darts
  , HasDarts'(..), HasDarts(..)
  , Core.tailOf, Core.headOf, Core.endPoints

  -- * Edges
  , HasEdges'(..), HasEdges(..)

  -- * Vertices
  , VertexIdIn, VertexId
  , HasVertices'(..), HasVertices(..)
  , Core.incidentEdges
  , Core.incomingEdges
  , Core.outgoingEdges
  , Core.neighboursOf
  , Core.nextIncidentEdge, Core.prevIncidentEdge
  , Core.nextIncidentEdgeFrom, Core.prevIncidentEdgeFrom

  -- * Faces
  , HasFaces'(..), HasFaces(..)
  , FaceIdIn(..), FaceId
  , leftFace, rightFace
  , boundaryDart, boundary, boundary', boundaryVertices
  , nextEdge, prevEdge

  -- * Dual Graph
  , Core.dual

  -- * Associated Data
  -- , HasDataOf(..)
  , Core.endPointDataOf
  ) where

import           Control.Lens
import           Hiraffe.Graph
import           Hiraffe.PlanarGraph.Core ( PlanarGraph
                                          , DualOf
                                          , World(..)
                                          , VertexIdIn(..), VertexId
                                          , FaceIdIn(..), FaceId
                                          , HasDataOf(..)

                                          )
import qualified Hiraffe.PlanarGraph.Core as Core
import           Hiraffe.PlanarGraph.Dart
import           Hiraffe.PlanarGraph.Dual
import           Hiraffe.PlanarGraph.IO
import           Hiraffe.PlanarGraph.Instance ()

--------------------------------------------------------------------------------
-- $setup
-- >>> import qualified Data.Vector as V
-- >>> import Control.Lens
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     myGraph :: PlanarGraph () Primal String String String
--     myGraph = planarGraph [ [ (Dart aA Negative, "a-")
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
--                           ] & vertexData .~ V.fromList ["u","v","w","x"]
--                             & faceData   .~ V.fromList ["f_3", "f_infty","f_1","f_2"]
--     showWithData     :: HasDataOf s i => s -> i -> (i, DataOf s i)
--     showWithData g i = (i, g^.dataOf i)
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlanarGraph/testG.png)


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
-- testG = planarGraph [ [ (Dart aA Negative, "a-")
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
--          IndexedTraversal (Dart s) (PlanarGraph s w v e f) (PlanarGraph s w v e' f) e e'
-- darts = conjoined traverse' (itraverse' . indexed)
--     where
--       traverse' :: Applicative g
--                 => (e -> g e') -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
--       traverse' = Core.dartData.traversed
--       itraverse' :: Applicative g
--                  => (Dart s -> e -> g e')
--                  -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
--       itraverse' = Core.traverseDarts
