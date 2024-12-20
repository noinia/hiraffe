--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing possibly disconnected planar graphs
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Type
  ( PlanarGraph
  , PlanarGraphF

  ) where


import           Control.Lens hiding (holes, holesOf, (.=))
import           Control.Monad.State
import           Data.Bifunctor (first, second)
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector.Mutable as MV
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           GHC.Generics (Generic)
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Vector.NonEmpty.Util
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Class
import           Hiraffe.PlanarGraph.Component
import           Hiraffe.PlanarGraph.Connected ( CPlanarGraph
                                               , VertexIdIn(..), VertexId
                                               , FaceIdIn(..), FaceId
                                               )
import           Hiraffe.PlanarGraph.Connected.Dual
import           Hiraffe.PlanarGraph.Connected.Instance ()
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.IO
import           Hiraffe.PlanarGraph.World

--------------------------------------------------------------------------------

-- | A PlanarGraph is essentially a bunch of connected Planargraphs; one for every
-- connected component. These graphs store the global ID's (darts, vertexId's, faceId's)
-- in their data values. This essentially gives us a mapping between the two.
--
-- note that a face may actually occur in multiple graphs, hence when we store
-- the edges to the the holes, we store the global edgeId's rather than the
-- 'local' edgeId (dart)'s.
--
-- invariant: the outerface has faceId 0
type PlanarGraph (s :: k) w v e f = PlanarGraphF s (PG w) v e f

-- | The Implementation of the PlanarGraph type, where the component is a parameter.
data PlanarGraphF (s   :: k)
                  (pg :: Type -> Type -> Type -> Type -> Type)
                  v e f = PlanarGraph
    { _components    :: NonEmptyVector (Component pg s)
    , _rawVertexData :: NonEmptyVector (Raw s (VertexId  (Wrap s)) v)
    , _rawDartData   :: NonEmptyVector (Raw s (Dart.Dart (Wrap s)) e)
    , _rawFaceData   :: NonEmptyVector (RawFace s f)
    } deriving (Functor,Generic)


-- | Just hte connected planar graph with the w and s arguments flipped
-- so that we can plug it into PlanarGraphF
newtype PG w s v e f = PG (CPlanarGraph s w v e f)
  deriving newtype (Show,Eq)




-- | Lens to access the connected components of a planar subdivision.
components :: Lens' (PlanarGraphF s pg v e f)
                    (NonEmptyVector (Component pg s))
components = lens _components (\ps cs -> ps { _components = cs })

-- | Lens to access the raw vertex data
rawVertexData :: Lens (PlanarGraphF s pg v e f)
                      (PlanarGraphF s pg v' e f)
                      (NonEmptyVector (Raw s (VertexId  (Wrap s)) v))
                      (NonEmptyVector (Raw s (VertexId  (Wrap s)) v'))
rawVertexData = lens _rawVertexData (\ps vxd -> ps { _rawVertexData = vxd })

-- | Lens to access the raw dart daat a
rawDartData :: Lens (PlanarGraphF s pg v e f) (PlanarGraphF s pg v e' f)
                    (NonEmptyVector (Raw s (Dart.Dart  (Wrap s)) e))
                    (NonEmptyVector (Raw s (Dart.Dart  (Wrap s)) e'))
rawDartData = lens _rawDartData (\ps vxd -> ps { _rawDartData = vxd })

-- | Access the raw face data
rawFaceData :: Lens (PlanarGraphF s pg v e f) (PlanarGraphF s pg v e f')
                    (NonEmptyVector (RawFace s f))    (NonEmptyVector (RawFace s f'))
rawFaceData = lens _rawFaceData (\ps vxd -> ps { _rawFaceData = vxd })

-- | Lens to access a particular component of the planar subdivision.
component    :: ComponentId s
             -> Lens' (PlanarGraphF s pg v e f) (Component pg s)
component ci = components.singular (ix $ unCI ci)

--------------------------------------------------------------------------------

-- | Given a "global" dart id, get the component and local dart info
asLocalD      :: Dart.Dart s -> PlanarGraphF s pg v e f
              -> (ComponentId s, Dart.Dart (Wrap s), component)
asLocalD d ps = let (Raw ci d' _) = ps^?!rawDartData.ix (fromEnum d)
                in (ci,d',ps^.component ci)

-- | Given a global vertexId, get the local info
asLocalV                 :: VertexId s -> PlanarGraphF s pg v e f
                         -> (ComponentId s, VertexId (Wrap s), component)
asLocalV (VertexId v) ps = let (Raw ci v' _) = ps^?!rawVertexData.ix v
                           in (ci,v',ps^.component ci)


-- -- | Get the local face and component from a given face.
-- asLocalF                          :: FaceId s -> PlanarGraphF s pg v e f
--                                   -> NonEmpty (ComponentId s, FaceId (Wrap s), Component s r)
-- asLocalF (FaceId (VertexId f)) ps = case ps^?!rawFaceData.ix f of
--       RawFace (Just (ci,f')) _        -> (ci,f',ps^.component ci) :| []
--       RawFace Nothing (FaceData hs _) -> toLocalF <$> NonEmpty.fromList (F.toList hs)
--   where
--     toLocalF d = let (ci,d',c) = asLocalD d ps in (ci,PG.leftFace d' c,c)


--------------------------------------------------------------------------------

instance HasVertices' (PlanarGraphF s pg v e f) where
  type Vertex   (PlanarGraphF s pg v e f) = v
  type VertexIx (PlanarGraphF s pg v e f) = VertexId s
  vertexAt u@(VertexId ui) = reindexed (const u) $ rawVertexData.iix ui <. dataVal
  numVertices = NonEmptyV.length . _rawVertexData
  {-# INLINE numVertices #-}

instance HasVertices (PlanarGraphF s pg v e f) (PlanarGraphF s pg v' e f)  where
  vertices = reindexed (VertexId :: Int -> VertexIx (PlanarGraphF s pg v e f))
           $ rawVertexData .> traversed1 <. dataVal
instance HasDarts' (PlanarGraphF s pg v e f) where

  type Dart   (PlanarGraphF s pg v e f) = e
  type DartIx (PlanarGraphF s pg v e f) = Dart.Dart s
  dartAt d = reindexed (const d) $ rawDartData.iix (fromEnum d) <. dataVal
  numDarts = NonEmptyV.length . _rawDartData
  {-# INLINE numDarts #-}

instance HasDarts (PlanarGraphF s pg v e f) (PlanarGraphF s pg v e' f)  where
  darts = reindexed (toEnum :: Int -> DartIx (PlanarGraphF s pg v e f))
        $ rawDartData .> itraversed <. dataVal

instance HasEdges' (PlanarGraphF s pg v e f) where
  type Edge   (PlanarGraphF s pg v e f) = e
  type EdgeIx (PlanarGraphF s pg v e f) = Dart.Dart s
  edgeAt d = reindexed (const d) $ rawDartData.iix (fromEnum d) <. dataVal
  numEdges = (`div` 2) . NonEmptyV.length . _rawDartData

instance HasEdges (PlanarGraphF s pg v e f) (PlanarGraphF s pg v e' f)  where
  -- edges = undefined
    -- reindexed (VertexId :: Int -> VertexIx (PlanarGraphF s pg v e f))
    --        $ rawDartData .> traversed1 <. dataVal
  -- TODO: we need some careful filtering like in planarGraph here as well

instance HasFaces' (PlanarGraphF s pg v e f) where
  type Face   (PlanarGraphF s pg v e f) = f
  type FaceIx (PlanarGraphF s pg v e f) = FaceId s
  faceAt fi = reindexed (const fi)
            $ rawFaceData .> iix (coerce fi) <. faceDataVal.fData
  numFaces = NonEmptyV.length . _rawFaceData
  {-# INLINE numFaces #-}

instance HasFaces (PlanarGraphF s pg v e f) (PlanarGraphF s pg v e f')  where
  faces = reindexed (coerce :: Int -> FaceIx (PlanarGraphF s pg v e f))
        $ rawFaceData .> traversed1 <. faceDataVal.fData

instance DiGraph_ (PlanarGraphF s pg v e f) where
  -- diGraphFromAdjacencyLists =

  tailOf d = ito $ \ps -> let (_,d',c) = asLocalD d ps
                              vi       = c^.tailOf d'
                          in (vi, ps^?!vertexAt vi)
    -- we look up the component c containing dart d. In this component d is actually known as d'.
    -- so we can use c^.tailOf d' to find the global index of the vertex we are looking for.
    -- we then simply look up this vertex
  headOf d = ito $ \ps -> let (_,d',c) = asLocalD d ps
                              vi       = c^.headOf d'
                          in (vi, ps^?!vertexAt vi)
    -- see tailOf; we use the same, except we use head rather than tail

  -- | All outgoing darts incident to vertex v, in counterclockwise order around v.
  outgoingDartsOf v = ifolding $ \ps -> let (_,v',c) = asLocalV v ps
                                        in foldMapOf (outgoingDartsOf v')
                                                     (\d -> [(d,ps^?!dartAt d)])
                                                     c
    -- we retrieve the component c containing vertex v. It's local name t here is v'.
    -- we then fold over the outoingDarts of v': i.e. this allows us to collect
    -- the global names (darts) that we care about. For each of those darts we then look up
    -- the actual data associated with it.

  twinDartOf d = twinOf d . to Just

instance BidirGraph_ (PlanarGraphF s pg v e f) where
  twinOf d = to $ const (Dart.twin d)
  getPositiveDart _ = id

-- TODO:  Component's should somehow link/remember how it gets its v values.
instance Graph_ (PlanarGraphF s pg v e f) where
  -- fromAdjacencyLists =

  neighboursOf v  = ifolding $ \ps -> let (_,v',c) = asLocalV v ps
                                      in foldMapOf (neighboursOf v')
                                                   (\w -> [(w,ps^?!vertexAt w)])
                                                   c
    -- same general approach as outGoingDartsOf
  incidentEdgesOf v = ifolding $ \ps -> let (_,v',c) = asLocalV v ps
                                        in foldMapOf (incidentEdgesOf v')
                                                     (\e -> [(e,ps^?!edgeAt e)])
                                                     c
    -- same general approach as outGoingDartsOf


-- instance PlanarGraph_ (PlanarGraphF s pg v e f) v where
--   -- dualGraph, (incidentFaceOf | leftFaceOf), rightFaceOf, prevDartOf, nextDartOf, boundaryDartOf, boundaryDartOf, boundaryDarts

-- instance PlaneGraph_ (PlanarGraphF s pg v e f) v where
--   -- TODO: fromEmbedding

-- instance PlanarGraphF component_ (PlanarGraphF component  s v e f) v where
