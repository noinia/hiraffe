{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.EdgeOracle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data structure to represent a planar graph with which we can test in
-- \(O(1)\) time if an edge between a pair of vertices exists.
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.EdgeOracle
  ( EdgeOracle
  , edgeOracle
  , buildEdgeOracle
  , hasEdge
  , findEdge
  , findDart
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Lens hiding ((.=))
import           Control.Monad (forM_)
import           Control.Monad.ST (ST, runST)
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Maybe (catMaybes, isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           HGeometry.Ext
import           Hiraffe.PlanarGraph.Core
import           Hiraffe.PlanarGraph.Dart

--------------------------------------------------------------------------------

-- | Edge Oracle:
--
-- main idea: store adjacency lists in such a way that we store an edge (u,v)
-- either in u's adjacency list or in v's. This can be done s.t. all adjacency
-- lists have length at most 6.
--
-- pre: the edges are considerd undirectional; i.e. the data stored at (u,v) is supposed
-- to be the same as at (v,u). find may arbitrarily return only one of the two values.
--
-- note: Every edge is stored exactly once (i.e. either at u or at v, but not both)
newtype EdgeOracle s w e = EdgeOracle (V.Vector (V.Vector (VertexIdIn w s :+ e)))
                         deriving (Show,Eq,Functor,Foldable,Traversable)

data DartData e = UToV e
                | VToU e
                | Both { uToV :: e -- ^ the data associated with the dart in this direction
                       , vToU :: e -- ^ the data associated with the reverse direction
                       }
                deriving (Show,Eq,Functor,Foldable,Traversable)

-- | Given a planar graph, construct an edge oracle. Given a pair of vertices
-- this allows us to efficiently find the dart representing this edge in the
-- graph.
--
-- pre: No self-loops and no multi-edges!!!
--
-- running time: \(O(n)\)
edgeOracle   :: PlanarGraph s w v e f -> EdgeOracle s w (DartId s)
edgeOracle g = buildEdgeOracle [ (v, mkAdjacency v <$> incidentEdges v g)
                               | v <- F.toList $ vertices' g
                               ]
  where
    mkAdjacency v d = otherVtx v d :+ d
    otherVtx v d = let u = tailOf d g in if u == v then headOf d g else u


withBothData              :: forall s w e. V.Vector (V.Vector (VertexIdIn w s :+ e))
                          -> EdgeOracle s w e
                          -> EdgeOracle s w (DartData e)
withBothData inAdj oracle = EdgeOracle oOut
  where
    (EdgeOracle os) = UToV <$> oracle
    -- | Finds the index of v in the adjacencylist of u
    find'     :: VertexIdIn w s -> VertexIdIn w s -> Maybe Int
    find' u v = V.findIndex ((== v) . (^.core)) $ os V.! (coerce u)

    oOut = runST $
           do out <- mapM V.thaw os
              V.iforM_ inAdj $ \ui adjU -> do
                let u = VertexId ui
                forM_ adjU $ \(v :+ eUV) ->
                  case find' u v of
                    Nothing -> case find' v u of
                      Just i  -> assign' out v i eUV -- (u,v) is stored at u
                      Nothing -> error $
                        "EdgeOracle.withBothData, edge" <> show (u,v) <> "not stored at all!"
                    Just _  -> pure () -- (u,v) is stored at v, so eUV is already stored there
              mapM V.unsafeFreeze out

    assign' out u i eVU = let adjU = out V.! (coerce u)
                          in MV.modify adjU (fmap (combine eVU)) i

    combine eVU = \case
      UToV eUV   -> Both eUV eVU
      VToU _     -> VToU eVU -- apparently we are updating VToU
      Both eUV _ -> Both eUV eVU
      -- I don't think these last two cases should occur.


-- | Builds an edge oracle that can be used to efficiently test if two vertices
-- are connected by an edge.
--
-- running time: \(O(n)\)
buildEdgeOracle        :: forall s w e f g. (Foldable f, Foldable g)
                       => f (VertexIdIn w s, g (VertexIdIn w s :+ e)) -> EdgeOracle s w e
buildEdgeOracle inAdj' = EdgeOracle $ V.create $ do
                          counts <- UV.thaw initCounts
                          marks  <- UMV.replicate (UMV.length counts) False
                          outV   <- MV.new (UMV.length counts)
                          build counts marks outV initQ
                          pure outV
    -- main idea: maintain a vector with counts; i.e. how many unprocessed
    -- vertices are adjacent to u, and a bit vector with marks to keep track if
    -- a vertex has been processed yet. When we process a vertex, we keep only
    -- the adjacencies of unprocessed vertices.
  where
    -- Convert to a vector representation
    inAdj :: V.Vector (V.Vector (VertexIdIn w s :+ e))
    inAdj = V.create $ do
              mv <- MV.new (length inAdj')
              forM_ inAdj' $ \(VertexId i,adjI) ->
                MV.write mv i (V.fromList . F.toList $ adjI)
              pure mv

    initCounts = V.convert . fmap GV.length $ inAdj
    -- initial vertices available for processing
    initQ = GV.ifoldr (\i k q -> if k <= 6 then i : q else q) [] initCounts

    -- | Construct the adjacencylist for vertex i. I.e. by retaining only adjacent
    -- vertices that have not been processed yet.
    extractAdj         :: UMV.MVector s' Bool -> Int
                       -> ST s' (V.Vector (VertexIdIn w s :+ e))
    extractAdj marks i = let p = fmap not . UMV.read marks . (^.core.unVertexId)
                         in GV.filterM  p $ inAdj V.! i

    -- | Decreases the number of adjacencies that vertex j has
    -- if it has <= 6 adjacencies left it has become available for processing
    decrease                          :: UMV.MVector s' Int
                                      -> (VertexIdIn w s :+ e')
                                      -> ST s' (Maybe Int)
    decrease counts (VertexId j :+ _) = do k <- UMV.read counts j
                                           let k'  = k - 1
                                           UMV.write counts j k'
                                           pure $ if k' <= 6 then Just j else Nothing

    -- The actual algorithm that builds the items
    build :: UMV.MVector s' Int -- ^ counts vector
          -> UMV.MVector s' Bool -- ^ Marks vector
          -> MV.MVector s' (V.Vector (VertexIdIn w s :+ e))
          -- ^ the output vector we are building
          -> [Int] -- ^ queue of vertices to process
          -> ST s' ()
    build _      _     _    []    = pure ()
    build counts marks outV (i:q) = do
             b <- UMV.read marks i
             nq <- if b then pure []
                        else do
                          adjI <- extractAdj marks i
                          MV.write outV i adjI
                          UMV.write marks i True
                          V.toList <$> mapM (decrease counts) adjI
             build counts marks outV (catMaybes nq <> q)



-- | Test if u and v are connected by an edge.
--
-- running time: \(O(1)\)
hasEdge     :: VertexIdIn w s -> VertexIdIn w s -> EdgeOracle s w a -> Bool
hasEdge u v = isJust . findEdge u v


-- | Find the edge data corresponding to edge (u,v) if such an edge exists.
--
--
-- running time: \(O(1)\)
findEdge :: VertexIdIn w s -> VertexIdIn w s -> EdgeOracle s w a -> Maybe a
findEdge  (VertexId u) (VertexId v) (EdgeOracle os) = find' u v <|> find' v u
  where
    find' j i = fmap (^.extra) . F.find (\(VertexId k :+ _) -> j == k) $ os V.! i

-- | Given a pair of vertices (u,v) returns the dart, oriented from u to v,
-- corresponding to these vertices.
--
-- running time: \(O(1)\)
findDart :: VertexIdIn w s -> VertexIdIn w s -> EdgeOracle s w (DartId s) -> Maybe (DartId s)
findDart (VertexId u) (VertexId v) (EdgeOracle os) = find' twin u v <|> find' id v u
  where
    -- looks up j in the adjacencylist of i and applies f to the result
    find' f j i = fmap (f . (^.extra)) . F.find (\(VertexId k :+ _) -> j == k) $ os V.! i
