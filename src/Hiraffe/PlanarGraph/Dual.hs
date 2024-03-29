--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Dual
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Stuff related to the dual of a planar graph
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Dual
  ( faces', faces
  , leftFace
  , nextEdge, prevEdge
  , boundaryDart, boundary, boundary'
  , boundaryVertices
  ) where

import           Control.Lens hiding ((.=))
import           Data.Maybe (fromMaybe)
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as V
import           Hiraffe.PlanarGraph.Core
import           Hiraffe.PlanarGraph.Dart

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> import Hiraffe.PlanarGraph.World
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> :{
-- let
--     dart             :: String -> Dart s
--     dart (arcC:dirS) = Dart (Arc arc') (read $ dirS <> "1")
--       where
--         arc' = fromMaybe (error "invalid arc") . lookup arcC $ zip "abcdefg" [0..]
--     dart _ = error "myDart"
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
--     myGraph :: PlanarGraph () Primal String String String
--     myGraph = planarGraph adjacencies
--                     & vertexData .~ V.unsafeFromList ["u","v","w","x"]
--                     & faceData   .~ V.unsafeFromList ["f_3", "f_infty","f_1","f_2"]
--     showWithData     :: HasDataOf s i => s -> i -> (i, DataOf s i)
--     showWithData g i = (i, g^.dataOf i)
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlanarGraph/testG.png)


-- | Enumerate all faces in the planar graph
faces' :: PlanarGraph s w v e f -> NonEmptyVector (FaceIdIn w s)
faces' = fmap FaceId . vertices' . view dual

-- | All faces with their face data.
--
-- >>> mapM_ print $ faces myGraph
-- (FaceId 0,"f_3")
-- (FaceId 1,"f_infty")
-- (FaceId 2,"f_1")
-- (FaceId 3,"f_2")
faces   :: PlanarGraph s w v e f -> NonEmptyVector (FaceIdIn w s, f)
faces g = V.zip (faces' g) (g^.faceData)

-- | The face to the left of the dart
--
--
-- running time: \(O(1)\).
leftFace     :: Dart s -> PlanarGraph s w v e f -> FaceIdIn w s
leftFace d g = FaceId . headOf d $ view dual g

-- | Get the next edge in order along the face (so ccw for internal faces, and cw for
-- external faces ) that is to the left of this dart.
----
-- running time: \(O(1)\).
nextEdge   :: Dart s -> PlanarGraph s w v e f -> Dart s
nextEdge d = prevIncidentEdgeFrom d . view dual

-- | Get the previous edge in order along the face (so ccw for internal faces, and cw
-- for external faces ) that is to the left of this dart.
--
--
--
-- running time: \(O(1)\).
prevEdge   :: Dart s -> PlanarGraph s w v e f -> Dart s
prevEdge d = nextIncidentEdgeFrom d . view dual

-- | Gets a dart bounding this face. I.e. a dart d such that the face lies to
-- the left of the dart.
--
-- >>> boundaryDart (FaceId $ VertexId 2) myGraph
-- Dart (Arc 1) +1
-- >>> showWithData myGraph $ boundaryDart (FaceId $ VertexId 2) myGraph
-- (Dart (Arc 1) +1,"b+")
-- >>> showWithData myGraph $ boundaryDart (FaceId $ VertexId 1) myGraph
-- (Dart (Arc 2) +1,"c+")
boundaryDart   :: FaceIdIn w s -> PlanarGraph s w v e f -> Dart s
boundaryDart f = V.head . boundary f

-- | The darts are reported in order along the face. This means that for internal faces
-- the darts are reported in *counter clockwise* order along the boundary, whereas for the
-- outer face the darts are reported in clockwise order.
--
-- >>> boundary (FaceId $ VertexId 2) myGraph
--
-- >>> mapM_ (print . showWithData myGraph) $ boundary (FaceId $ VertexId 2) myGraph
-- (Dart (Arc 2) -1,"c-")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 1) -1,"b-")
-- >>> boundary (FaceId $ VertexId 1) myGraph
-- [Dart (Arc 2) +1,Dart (Arc 4) +1,Dart (Arc 1) -1,Dart (Arc 0) +1]
-- >>> mapM_ (print . showWithData myGraph) $ boundary (FaceId $ VertexId 1) myGraph
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 2) +1,"c+")
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundary            :: FaceIdIn w s -> PlanarGraph s w v e f -> NonEmptyVector (Dart s)
boundary (FaceId v) = incidentEdges v . view dual

-- | Given a dart d, generates the darts bounding the face that is to
-- the left of the given dart. The darts are reported in order along
-- the face. This means that for internal faces the darts are reported
-- in *counter clockwise* order along the boundary, whereas for the outer face
-- the darts are reported in clockwise order.
--
-- >>> mapM_ (print . showWithData myGraph) $ boundary' (dart "b+") myGraph
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 2) -1,"c-")
--
-- \(O(k)\), where \(k\) is the number of darts reported
boundary'     :: Dart s -> PlanarGraph s w v e f -> NonEmptyVector (Dart s)
boundary' d g = fromMaybe (error "boundary'")  . rotateTo d $ boundary (leftFace d g) g
  where
    rotateTo     :: Eq a => a -> NonEmptyVector a -> Maybe (NonEmptyVector a)
    rotateTo x v = f <$> V.elemIndex x v
      where
        f i = let (a,b) = V.splitAt i v in V.unsafeFromVector $ b <> a


-- | The vertices bounding this face, for internal faces in counter clockwise
-- order, for the outer face in clockwise order.
--
-- >>> mapM_ (print . showWithData myGraph) $ boundaryVertices (FaceId $ VertexId 2) myGraph
-- (VertexId 2,"w")
-- (VertexId 0,"u")
-- (VertexId 1,"v")
-- >>> mapM_ (print . showWithData myGraph) $ boundaryVertices (FaceId $ VertexId 1) myGraph
-- (VertexId 0,"u")
-- (VertexId 2,"v")
-- (VertexId 1,"w")
-- (VertexId 0,"u")
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices     :: FaceIdIn w s -> PlanarGraph s w v e f -> NonEmptyVector (VertexIdIn w s)
boundaryVertices f g = flip tailOf g <$> boundary f g

-- -- | Gets the next dart along the face
-- nextDart     :: Dart s -> PlanarGraph s w v e f -> Dart s
-- nextDart d g = f rightFace e
