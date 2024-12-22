{-# LANGUAGE UndecidableInstances #-}
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
  , PlanarGraphF(..)
  , Component
  , fromConnected'
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Functor.Apply (Apply)
import qualified Data.Functor.Apply as Apply
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Semigroup.Traversable
import qualified Data.Sequence as Seq
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           GHC.Generics (Generic)
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Class
import           Hiraffe.PlanarGraph.Component
import           Hiraffe.PlanarGraph.Connected ( CPlanarGraph
                                               , VertexIdIn(..), VertexId
                                               , FaceIdIn(..), FaceId
                                               )
import qualified Hiraffe.PlanarGraph.Connected.Core as Core
import           Hiraffe.PlanarGraph.Connected.Instance ()
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.World


--------------------------------------------------------------------------------

-- | A connected component.
--
-- For every face f, and every hole in this face, the facedata points to a dart
-- d on the hole s.t. this dart has the face f on its left. i.e.
-- leftFace d = f
type Component (planarGraph :: Type -> Type -> Type -> Type -> Type) (s :: k) =
  planarGraph (Wrap s) (VertexId s) (Dart.Dart s) (FaceId s)


-- | A PlanarGraph is essentially a bunch of connected Planargraphs; one for every
-- connected component. These graphs store the global ID's (darts, vertexId's, faceId's)
-- in their data values. This essentially gives us a mapping between the two.
--
-- note that a face may actually occur in multiple graphs, hence when we store
-- the edges to the the holes, we store the global edgeId's rather than the
-- 'local' edgeId (dart)'s.
--
-- invariant: the outerface has faceId 0
type PlanarGraph w (s :: k) v e f = PlanarGraphF (CPlanarGraph w) s v e f

-- | The Implementation of the PlanarGraph type, where the component is a parameter.
data PlanarGraphF (pg :: Type -> Type -> Type -> Type -> Type)
                  (s   :: k)
                  v e f = PlanarGraph
    { _components    :: NonEmptyVector (Component pg s)
    , _rawVertexData :: NonEmptyVector (Raw s (VertexId  (Wrap s)) v)
    , _rawDartData   :: NonEmptyVector (Raw s (Dart.Dart (Wrap s)) e)
    , _rawFaceData   :: NonEmptyVector (RawFace s f)
    } deriving (Functor,Generic)


-- | Lens to access the connected components of a planar subdivision.
components :: Lens' (PlanarGraphF pg s v e f)
                    (NonEmptyVector (Component pg s))
components = lens _components (\ps cs -> ps { _components = cs })

-- | Lens to access the raw vertex data
rawVertexData :: Lens (PlanarGraphF pg s v e f)
                      (PlanarGraphF pg s v' e f)
                      (NonEmptyVector (Raw s (VertexId  (Wrap s)) v))
                      (NonEmptyVector (Raw s (VertexId  (Wrap s)) v'))
rawVertexData = lens _rawVertexData (\ps vxd -> ps { _rawVertexData = vxd })

-- | Lens to access the raw dart daat a
rawDartData :: Lens (PlanarGraphF pg s v e f) (PlanarGraphF pg s v e' f)
                    (NonEmptyVector (Raw s (Dart.Dart  (Wrap s)) e))
                    (NonEmptyVector (Raw s (Dart.Dart  (Wrap s)) e'))
rawDartData = lens _rawDartData (\ps vxd -> ps { _rawDartData = vxd })

-- | Access the raw face data
rawFaceData :: Lens (PlanarGraphF pg s v e f) (PlanarGraphF pg s v e f')
                    (NonEmptyVector (RawFace s f))    (NonEmptyVector (RawFace s f'))
rawFaceData = lens _rawFaceData (\ps vxd -> ps { _rawFaceData = vxd })

-- | Lens to access a particular component of the planar subdivision.
component    :: ComponentId s
             -> Lens' (PlanarGraphF pg s v e f) (Component pg s)
component ci = components.singular (ix $ unCI ci)

--------------------------------------------------------------------------------

-- | Given a "global" dart id, get the component and local dart info
asLocalD      :: Dart.Dart s -> PlanarGraphF pg s v e f
              -> (ComponentId s, Dart.Dart (Wrap s), Component pg s)
asLocalD d ps = let (Raw ci d' _) = ps^?!rawDartData.ix (fromEnum d)
                in (ci,d',ps^.component ci)

-- | Given a global vertexId, get the local info
asLocalV                 :: VertexId s -> PlanarGraphF pg s v e f
                         -> (ComponentId s, VertexId (Wrap s), Component pg s)
asLocalV (VertexId v) ps = let (Raw ci v' _) = ps^?!rawVertexData.ix v
                           in (ci,v',ps^.component ci)


-- -- | Get the local face and component from a given face.
-- asLocalF                          :: FaceId s -> PlanarGraphF pg s v e f
--                                   -> NonEmpty (ComponentId s, FaceId (Wrap s), Component s r)
-- asLocalF (FaceId (VertexId f)) ps = case ps^?!rawFaceData.ix f of
--       RawFace (Just (ci,f')) _        -> (ci,f',ps^.component ci) :| []
--       RawFace Nothing (FaceData hs _) -> toLocalF <$> NonEmpty.fromList (F.toList hs)
--   where
--     toLocalF d = let (ci,d',c) = asLocalD d ps in (ci,PG.leftFace d' c,c)

--------------------------------------------------------------------------------

instance HasVertices' (PlanarGraphF pg s v e f) where
  type Vertex   (PlanarGraphF pg s v e f) = v
  type VertexIx (PlanarGraphF pg s v e f) = VertexId s
  vertexAt u@(VertexId ui) = reindexed (const u) $ rawVertexData.iix ui <. dataVal
  numVertices = NonEmptyV.length . _rawVertexData
  {-# INLINE numVertices #-}

instance HasVertices (PlanarGraphF pg s v e f) (PlanarGraphF pg s v' e f)  where
  vertices = reindexed (VertexId :: Int -> VertexIx (PlanarGraphF pg s v e f))
           $ rawVertexData .> traversed1 <. dataVal
instance HasDarts' (PlanarGraphF pg s v e f) where

  type Dart   (PlanarGraphF pg s v e f) = e
  type DartIx (PlanarGraphF pg s v e f) = Dart.Dart s
  dartAt d = reindexed (const d) $ rawDartData.iix (fromEnum d) <. dataVal
  numDarts = NonEmptyV.length . _rawDartData
  {-# INLINE numDarts #-}

instance HasDarts (PlanarGraphF pg s v e f) (PlanarGraphF pg s v e' f)  where
  darts = reindexed (toEnum :: Int -> DartIx (PlanarGraphF pg s v e f))
        $ rawDartData .> itraversed <. dataVal

instance HasEdges' (PlanarGraphF pg s v e f) where
  type Edge   (PlanarGraphF pg s v e f) = e
  type EdgeIx (PlanarGraphF pg s v e f) = Dart.Dart s
  edgeAt d = reindexed (const d) $ rawDartData.iix (fromEnum d) <. dataVal
  numEdges = (`div` 2) . NonEmptyV.length . _rawDartData

instance HasEdges (PlanarGraphF pg s v e f) (PlanarGraphF pg s v e' f)  where
  edges = itraverse'.indexed
    where
      itraverse' f = Apply.unwrapApplicative
                   . itraverse1' (\d e -> Apply.WrapApplicative $ f d e)

      itraverse1'      :: Apply g
                       => (Dart.Dart s -> e -> g e')
                       -> PlanarGraphF pg s v e f -> g (PlanarGraphF pg s v e' f)
      itraverse1' f pg = pg&rawDartData %%~ itraverseEdges1 f

-- | itraverse the edges; i.e. makes sure to only apply our function to the positive darts.
itraverseEdges1     :: forall g s a e e'. Apply g
                    => (Dart.Dart s -> e -> g e')
                    -> NonEmptyVector (Raw s a e)
                    -> g (NonEmptyVector (Raw s a e'))
itraverseEdges1 f v = copyPositives <$> gv'
  where
    -- We collect only the positive darts, and apply the given function on them. We tag
    -- the result with the dart they correspond to (or rather, the corresponding index)
    gv' :: g (NonEmpty (Int, e'))
    gv' = sequence1 . NonEmpty.fromList $ NonEmptyV.ifoldr applyF [] v
    applyF i raw xs = let d = toEnum i
                      in if Dart.isPositive d
                         then ((i,) <$> f d (raw^.dataVal)) : xs
                         else xs

    -- We simultaneously scan through the original vector and the result of processing the
    -- positive darts. For positive darts we simply take the value as computed before. For
    -- negative darts, we lookup the value that we computed for their corresponding twin.
    -- i.e. by tying the knot.
    copyPositives           :: NonEmpty.NonEmpty (Int, e')
                            -> NonEmptyVector (Raw s a e')
    copyPositives positives = let (_,v') = imapAccumL (setDartValue v') (F.toList positives) v
                              in v'

    setDartValue v' i xs raw = case xs of
        (j,e') : xs' | i == j -> (xs', raw&dataVal .~ e')
        _                     -> let i' = fromEnum . Dart.twin . toEnum $ i
                                 in (xs, raw&dataVal .~ (v' NonEmptyV.! i')^.dataVal)


instance HasFaces' (PlanarGraphF pg s v e f) where
  type Face   (PlanarGraphF pg s v e f) = f
  type FaceIx (PlanarGraphF pg s v e f) = FaceId s
  faceAt fi = reindexed (const fi)
            $ rawFaceData .> iix (coerce fi) <. faceDataVal.fData
  numFaces = NonEmptyV.length . _rawFaceData
  {-# INLINE numFaces #-}

instance HasFaces (PlanarGraphF pg s v e f) (PlanarGraphF pg s v e f')  where
  faces = reindexed (coerce :: Int -> FaceIx (PlanarGraphF pg s v e f))
        $ rawFaceData .> traversed1 <. faceDataVal.fData


-- DartIx
--                              (pg
--                                 (Hiraffe.PlanarGraph.Component.Wrap' s)
--                                 (VertexId s)
--                                 (Dart.Dart s)
--                                 (FaceId s))
--                      with:

type IsComponent pg s = ( DartIx   (Component pg s) ~ Dart.Dart (Wrap' s)
                        , VertexIx (Component pg s) ~ VertexId (Wrap' s)
                        , Dart     (Component pg s) ~ Dart.Dart s
                        , Vertex   (Component pg s) ~ VertexId s
                        -- , FaceId (Component pg s) ~ FaceId s

                        )

instance ( BidirGraph_ (Component pg s)
         , IsComponent pg s
         ) => DiGraph_ (PlanarGraphF pg s v e f) where

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

instance ( IsComponent pg s
         , BidirGraph_ (Component pg s)
         ) => BidirGraph_ (PlanarGraphF pg s v e f) where
  twinOf d = to $ const (Dart.twin d)
  getPositiveDart _ = id


instance ( Graph_ (Component pg s)
         , IsComponent pg s
         , EdgeIx   (Component pg s) ~ Dart.Dart (Wrap' s)
         , Edge     (Component pg s) ~ Dart.Dart s
         ) => Graph_ (PlanarGraphF pg s v e f) where
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

-- instance PlanarGraph_ (PlanarGraphF pg s v e f) v where
--   -- dualGraph, (incidentFaceOf | leftFaceOf), rightFaceOf, prevDartOf, nextDartOf, boundaryDartOf, boundaryDartOf, boundaryDarts

-- instance PlaneGraph_ (PlanarGraphF pg s v e f) v where
--   -- TODO: fromEmbedding

-- instance PlanarGraph_ component_ (PlanarGraphF component  s v e f) v where
-- type DualGraphOf (CPlanarGraph w s v e f) = CPlanarGraph (DualOf w) s f e v



--------------------------------------------------------------------------------

{-
-- | Constructs a PlanarGraph from a connected planar graph (i.e. a single component graph)
--
-- runningTime: \(O(n)\)
fromConnected   :: forall s v e f. (Ord r, Num r)
                => CPlanarGraph w s v e f -> PlanarGraph w s v e f
fromConnected g = fromConnected' g (PG.outerFaceDart g)

 -}

-- | Given a (connected) PlanarGraph and a dart that has the outerface on its left
-- | Constructs a planarsubdivision
--
-- runningTime: \(O(n)\)
fromConnected'        :: forall s v e f.
                        CPlanarGraph Primal s v e f
                     -> Dart.Dart s
                     -> PlanarGraph Primal s v e f
fromConnected' g ofD = PlanarGraph (NonEmptyV.singleton $ Core.unsafeChangeS g') vd ed fd
  where
    c = ComponentId 0
    vd = imap (\i v -> Raw c (VertexId i) v)                   $ g^.Core.vertexData
    ed = NonEmptyV.zipWith (\d dd  -> Raw c d dd) allDarts     $ g^.Core.dartData
    fd = imap (\i f -> RawFace (mkFaceIdx i) (mkFaceData i f)) $ g^.Core.faceData

    g' :: CPlanarGraph Primal s (VertexId s) (Dart.Dart s) (FaceId s)
    g' = g&Core.faceData    %~ imap (\i _ -> mkFaceId $ flipID i)
          &Core.vertexData  %~ imap (\i _ -> VertexId i)
          &Core.dartData    .~ allDarts

    allDarts :: forall s'. NonEmptyVector (Dart.Dart s')
    allDarts = fromMaybe (error "allDarts'. absurd")
             $ NonEmptyV.fromListN (numDarts g) Dart.allDarts

    -- make sure the outerFaceId is 0
    (oF, ofData) = g^.leftFaceOf ofD.withIndex

    mkFaceIdx i | i == 0    = Nothing
                | otherwise = Just (c,mkFaceId . flipID $ i)

    -- at index i we are storing the outerface
    mkFaceData                        :: Int -> f -> FaceData (Dart.Dart s) f
    mkFaceData i f | i == 0           = FaceData (Seq.singleton ofD) ofData
                   | i == (coerce oF) = FaceData mempty              (g^?!faceAt (mkFaceId @s 0))
                   | otherwise        = FaceData mempty              f

    mkFaceId :: forall s'. Int -> FaceId s'
    mkFaceId = FaceId . VertexId

    flipID i | i == 0           = (coerce oF)
             | i == (coerce oF) = 0
             | otherwise        = i
