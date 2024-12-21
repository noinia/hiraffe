module Hiraffe.PlanarGraph.Build
  ( fromConnected'
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Control.Monad.State
import           Data.Bifunctor (first, second)
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import qualified Data.Functor.Apply as Apply
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe)
import           Data.Semigroup.Traversable
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
import qualified Hiraffe.PlanarGraph.Connected.Core as Core
import           Hiraffe.PlanarGraph.Connected.Dual
import           Hiraffe.PlanarGraph.Connected.Instance ()
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.IO
import           Hiraffe.PlanarGraph.Type
import           Hiraffe.PlanarGraph.World
import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | Given a (connected) PlanarGraph and a dart that has the outerface on its left
-- | Constructs a planarsubdivision
--
-- runningTime: \(O(n)\)
fromConnected'        :: forall s v e f.
                        CPlanarGraph Primal s v e f
                     -> Dart.Dart s
                     -> PlanarGraph Primal s v e f
fromConnected' g ofD = PlanarGraph (NonEmptyV.singleton $ coerce' g') vd ed fd
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
    mkFaceId = coerce

    flipID i | i == 0           = (coerce oF)
             | i == (coerce oF) = 0
             | otherwise        = i

-- | coerce the compoenent into a Wrap
coerce' :: forall s' v' e' f'.
           CPlanarGraph Primal s' v' e' f' -> CPlanarGraph Primal (Wrap s') v' e' f'
coerce' = unsafeCoerce
