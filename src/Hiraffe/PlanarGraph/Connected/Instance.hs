{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Connected.Instance
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The instances of the various classes for our planar grpah implementation.
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Connected.Instance
  (
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import qualified Data.Functor.Apply as Apply
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Traversable
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as V
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Class
import           Hiraffe.PlanarGraph.Connected.Core (PlanarGraph, VertexIdIn, FaceIdIn)
import qualified Hiraffe.PlanarGraph.Connected.Core as Core
import qualified Hiraffe.PlanarGraph.Dart as Dart
import qualified Hiraffe.PlanarGraph.Connected.Dual as Dual
import qualified Hiraffe.PlanarGraph.IO as IO
import           Hiraffe.PlanarGraph.World

--------------------------------------------------------------------------------

instance HasVertices' (PlanarGraph s w v e f) where
  type Vertex   (PlanarGraph s w v e f) = v
  type VertexIx (PlanarGraph s w v e f) = VertexIdIn w s

  vertexAt v = Core.dataOf v
  numVertices = Core.numVertices

instance HasVertices (PlanarGraph s w v e f) (PlanarGraph s w v' e f) where
  vertices = conjoined traverse' (itraverseV . indexed)
    where
      traverse' :: Apply g
                => (v -> g v') -> PlanarGraph s w v e f -> g (PlanarGraph s w v' e f)
      traverse' = Core.vertexData.traversed1
      itraverseV :: Apply g
                 => (VertexIdIn w s -> v -> g v')
                 -> PlanarGraph s w v e f -> g (PlanarGraph s w v' e f)
      itraverseV = Core.traverseVertices

----------------------------------------

instance HasDarts' (PlanarGraph s w v e f) where
  type Dart   (PlanarGraph s w v e f) = e
  type DartIx (PlanarGraph s w v e f) = Dart.Dart s
  dartAt d = Core.dataOf d
  numDarts = Core.numDarts

instance HasDarts (PlanarGraph s w v e f) (PlanarGraph s w v e' f) where
  darts = conjoined (Core.dartData.traversed) (itraverseDarts' . indexed)
    where
      itraverseDarts' :: Applicative g
                      => (Core.DartId s -> e -> g e')
                      -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
      itraverseDarts' f = Apply.unwrapApplicative
                        . Core.traverseDarts (\d e -> Apply.WrapApplicative $ f d e)

----------------------------------------

instance HasEdges' (PlanarGraph s w v e f) where
  type Edge   (PlanarGraph s w v e f) = e
  type EdgeIx (PlanarGraph s w v e f) = Dart.Dart s
  edgeAt d = edgeAtLens d
  numEdges = Core.numEdges

-- | Edge at lens, note that it actually modifies *both* the data associated with both the
-- positive and the negative occurance of the given dart.
edgeAtLens   :: Dart.Dart s -> IndexedLens' (Dart.Dart s) (PlanarGraph s w v e f) e
edgeAtLens d = ilens (\g -> (d, get'' g)) set''
  where
    d' = Dart.asPositive d
    get'' g   = g^?!dartAt d'
    set'' g e = g&dartAt d' .~ e
                 &dartAt d  .~ e

instance HasEdges (PlanarGraph s w v e f) (PlanarGraph s w v e' f) where
  edges = itraverse'.indexed
    where
      itraverse' f = Apply.unwrapApplicative
                   . itraverse1' (\d e -> Apply.WrapApplicative $ f d e)

      itraverse1'      :: Apply g
                       => (Dart.Dart s -> e -> g e')
                       -> PlanarGraph s w v e f -> g (PlanarGraph s w v e' f)
      itraverse1' f pg = pg&Core.dartData %%~ itraverseEdges1 f

-- | itraverse the edges; i.e. makes sure to only apply our function to the positive darts.
itraverseEdges1     :: forall g s e e'. Apply g
                    => (Dart.Dart s -> e -> g e') -> NonEmptyVector e -> g (NonEmptyVector e')
itraverseEdges1 f v = copyPositives <$> gv'
  where
    -- We collect only the positive darts, and apply the given function on them. We tag
    -- the result with the dart they correspond to (or rather, the corresponding index)
    gv' :: g (NonEmpty.NonEmpty (Int, e'))
    gv' = sequence1 . NonEmpty.fromList $ V.ifoldr applyF [] v
    applyF i e xs = let d = toEnum i
                    in if Dart.isPositive d
                       then ((i,) <$> f d e) : xs
                       else xs

    -- We simultaneously scan through the original vector and the result of processing the
    -- positive darts. For positive darts we simply take the value as computed before. For
    -- negative darts, we lookup the value that we computed for their corresponding twin.
    -- i.e. by tying the knot.
    copyPositives           :: NonEmpty.NonEmpty (Int, e') -> NonEmptyVector e'
    copyPositives positives = let (_,v') = imapAccumL (setDartValue v') (F.toList positives) v in v'
    setDartValue v' i xs _ = case xs of
                               (j,e') : xs' | i == j -> (xs',e')
                               _                     -> let i' = fromEnum . Dart.twin . toEnum $ i
                                                        in (xs, v' V.! i')

-- TODO: introduce some rewrite rules for folds; since then we don't have to reconstruct
-- output vectors.

----------------------------------------

instance HasFaces' (PlanarGraph s w v e f) where
  type Face   (PlanarGraph s w v e f) = f
  type FaceIx (PlanarGraph s w v e f) = FaceIdIn w s
  faceAt fi = Core.dataOf fi
  numFaces = Core.numFaces

instance HasFaces (PlanarGraph s w v e f) (PlanarGraph s w v e f') where
  faces = conjoined (Core.faceData.traversed1) (Core.traverseFaces.indexed)

--------------------------------------------------------------------------------

instance DiGraph_ (PlanarGraph s w v e f) where
  type DiGraphFromAdjListExtraConstraints (PlanarGraph s w v e f) h = (f ~ (), Foldable1 h)

  -- | The vertices are expected to have their adjacencies in CCW order.
  diGraphFromAdjacencyLists = IO.fromAdjacencyLists

  endPoints = flip Core.endPoints

  outNeighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) v
      asFold  = folding  $ \g -> (\v ->     g^?! vertexAt v)  <$> Core.neighboursOf u g
      asIFold = ifolding $ \g -> (\v -> (v, g^?! vertexAt v)) <$> Core.neighboursOf u g
  {-# INLINE outNeighboursOf #-}

  outgoingDartsOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) e
      asFold  = folding  $ \g -> (\d ->     g^?! edgeAt d)  <$> Core.outgoingEdges u g
      asIFold = ifolding $ \g -> (\d -> (d, g^?! edgeAt d)) <$> Core.outgoingEdges u g
  {-# INLINE outgoingDartsOf#-}

  twinDartOf d = twinOf d . to Just

instance BidirGraph_ (PlanarGraph s w v e f) where
  twinOf d = to $ const (Dart.twin d)
  getPositiveDart _ = id

instance Graph_ (PlanarGraph s w v e f) where
  type GraphFromAdjListExtraConstraints (PlanarGraph s w v e f) h = (f ~ (), Foldable1 h)

  -- | The vertices are expected to have their adjacencies in CCW order.
  fromAdjacencyLists = IO.fromAdjacencyLists

  neighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) v
      asFold  = folding  $ \g -> (\v ->     g^?! vertexAt v)  <$> Core.neighboursOf u g
      asIFold = ifolding $ \g -> (\v -> (v, g^?! vertexAt v)) <$> Core.neighboursOf u g
  {-# INLINE neighboursOf #-}

  incidentEdgesOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (PlanarGraph s w v e f) e
      asFold  = folding  $ \g -> (\d ->     g^?! edgeAt d)  <$> Core.outgoingEdges u g
      asIFold = ifolding $ \g -> (\d -> (d, g^?! edgeAt d)) <$> Core.outgoingEdges u g
  {-# INLINE incidentEdgesOf #-}


instance PlanarGraph_ (PlanarGraph s w v e f) where
  type DualGraphOf (PlanarGraph s w v e f) = PlanarGraph s (DualOf w) f e v

  dualGraph = view Core.dual

  leftFaceOf d = \paFb gr -> let fi = Dual.leftFace d gr
                             in singular (faceAt fi) paFb gr

  rightFaceOf d = leftFaceOf (Dart.twin d)
    -- \paFb gr -> let fi = Dual.rightFace d gr
    --                           in singular (faceAt fi) paFb gr

  nextDartOf d = \paFb gr -> let d' = Dual.nextEdge d gr
                             in singular (dartAt d') paFb gr

  prevDartOf d = \paFb gr -> let d' = Dual.prevEdge d gr
                             in singular (dartAt d') paFb gr

  boundaryDartOf d = \paFb gr -> let d' = Dual.boundaryDart d gr
                                 in singular (dartAt d') paFb gr

  boundaryDarts = Dual.boundary
