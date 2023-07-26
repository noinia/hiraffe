{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Hiraffe.PlanarGraphSpec where

import           Control.Lens (view,_3,ifoldMapOf)
import           Data.Bifunctor
import qualified Data.Foldable as F
import qualified Data.Map.Strict as SM
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.YAML
import           HGeometry.Combinatorial.Util
import           HGeometry.Permutation (toCycleRep)
import           HGeometry.YAML
import           Hiraffe.Instances ()
import           Hiraffe.PlanarGraph ( PlanarGraph, VertexId, DartId, VertexIdIn(..)
                                     , neighboursOf, planarGraph', vertices
                                     )
import qualified Hiraffe.PlanarGraph as PlanarGraph
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.IO
import qualified System.File.OsPath as File
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------
data TestG

type Vertex = VertexId TestG

instance ToYAML () where
  toYAML _ = toYAML ([] :: [Int])
instance FromYAML () where
  parseYAML n = parseYAML n >>= \case
                  ([] :: [Int]) -> pure ()
                  _             -> fail "parse () failed"

-- | Report all adjacnecies from g missing in h
missingAdjacencies     :: PlanarGraph s w v e f -> PlanarGraph s w v e f
                       -> [(VertexIdIn w s , VertexIdIn w s)]
missingAdjacencies g h = ifoldMapOf vertices f g
  -- concatMap f $ g^..vertices.traverse.index
  where
    f u _ = let adjUh = S.fromList . F.toList $ neighboursOf u h
            in F.toList . fmap (u,) . V.filter (`S.notMember` adjUh) $ neighboursOf u g

sameGraphs s g h = do
    describe ("Same Adjacencies " <> s) $ do
      it "Missing edges from g in h" $
          (missingAdjacencies g h) `shouldBe` []
      it "Missing edges from h in g" $
          (missingAdjacencies h g) `shouldBe` []

spec :: Spec
spec = describe "PlanarGraph spec" $ do
    -- sameGraphs "testEdges" (fromAdjacencyLists testEdges) (fromAdjacencyListsOld testEdges)
    prop "quickheck Dart:  (toEnum (fromEnum d)) = d" $
      \(d :: DartId TestG) -> toEnum (fromEnum d) `shouldBe` d
    prop "quickheck Dart: fromEnum (toEnum i) = i" $
      \(NonNegative i) -> fromEnum ((toEnum i) :: DartId TestG) `shouldBe` i
    it "encode yaml test" $ do
      b <- File.readFile' [osp|data/PlanarGraph/myGraph.yaml|]
      encodeYAML (fromAdjacencyLists testEdges) `shouldBe` b
    it "decode yaml test" $ do
      (first show <$> decodeYAMLFile [osp|Data/PlanarGraph/myGraph.yaml|])
      `shouldReturn`
      (Right $ fromAdjacencyLists testEdges)


testEdges :: [(Vertex, Int, [(Vertex, Text.Text ) ])]
testEdges = map (\(i,vs) -> (VertexId i, i,
                              map (\j -> (VertexId j, Text.pack $ "edge" <> show (i,j))) vs))
            [ (0, [1])
            , (1, [0,2,4])
            , (2, [1,3,4])
            , (3, [2,5])
            , (4, [1,2,5])
            , (5, [3,4])
            ]

-- testGraph = fromAdjacencyLists testEdges

-- enccode = let g =
--           in encodeYAMLFile

--------------------------------------------------------------------------------


-- - m: a Map, mapping edges, represented by a pair of vertexId's (u,v) with
--            u < v, to arcId's.
-- - a: the next available unused arcID
-- - x: the data value we are interested in computing
type STR' s b = STR (SM.Map (VertexId s,VertexId s) Int) Int b

-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter clockwise order.
--
-- running time: $O(n \log n)$.
fromAdjacencyListsOld      :: forall s f.(Foldable f, Functor f)
                        => [(VertexId s, f (VertexId s))]
                        -> PlanarGraph s PlanarGraph.Primal () () ()
fromAdjacencyListsOld adjM = planarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ perm
    perm = view (_3) . foldr toOrbit (STR mempty 0 mempty) $ adjM


    -- | Given a vertex with its adjacent vertices (u,vs) (in CCW order) convert this
    -- vertex with its adjacent vertices into an Orbit
    toOrbit                     :: Foldable f
                                => (VertexId s, f (VertexId s))
                                -> STR' s [[DartId s]]
                                -> STR' s [[DartId s]]
    toOrbit (u,vs) (STR m a dss) =
      let (STR m' a' ds') = foldr (toDart . (u,)) (STR m a mempty) . F.toList $ vs
      in STR m' a' (ds':dss)


    -- | Given an edge (u,v) and a triplet (m,a,ds) we construct a new dart
    -- representing this edge.
    toDart                    :: (VertexId s,VertexId s)
                              -> STR' s [DartId s]
                              -> STR' s [DartId s]
    toDart (u,v) (STR m a ds) = let dir = if u < v then PlanarGraph.Positive else PlanarGraph.Negative
                                    t'  = (min u v, max u v)
                               in case SM.lookup t' m of
      Just a' -> STR m                  a     (Dart.Dart (Dart.Arc a') dir : ds)
      Nothing -> STR (SM.insert t' a m) (a+1) (Dart.Dart (Dart.Arc a)  dir : ds)
