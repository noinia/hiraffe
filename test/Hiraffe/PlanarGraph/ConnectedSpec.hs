{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Hiraffe.PlanarGraph.ConnectedSpec where

import           Control.Lens ( view,_3,ifoldMapOf,(^..), asIndex, lengthOf, toListOf
                              , withIndex, (^?!)
                              )
import           Control.Monad
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as SM
import qualified Data.Set as S
import qualified Data.Text as Text
import           Data.YAML
import           HGeometry.Combinatorial.Util
import           HGeometry.Permutation (toCycleRep)
import           HGeometry.YAML
import           Hiraffe.Instances ()
import           Hiraffe.PlanarGraph ( CPlanarGraph, VertexId, DartId, VertexIdIn(..)
                                     , neighboursOf, cPlanarGraph', vertices
                                     , darts, edges
                                     , outNeighboursOf
                                     , outgoingDartsOf
                                     , diGraphFromAdjacencyLists
                                     , dartAt, endPoints
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
missingAdjacencies     :: CPlanarGraph w s v e f
                       -> CPlanarGraph w s v' e' f'
                       -> [(VertexIdIn w s , VertexIdIn w s)]
missingAdjacencies g h = ifoldMapOf vertices f g
  where
    f u _ = let adjUh = S.fromList $ h^..neighboursOf u.asIndex
            in F.toList . fmap (u,) . filter (`S.notMember` adjUh) $ h^..neighboursOf u.asIndex

sameGraphs s g h = do
    describe ("Same Adjacencies " <> s) $ do
      it "Missing edges from g in h" $
          (missingAdjacencies g h) `shouldBe` []
      it "Missing edges from h in g" $
          (missingAdjacencies h g) `shouldBe` []
      -- it "same embedding" $
      --   (g^.embedding) `shouldBe` (h^.embedding) -- apparently this is not true


spec :: Spec
spec = describe "PlanarGraph.Connected spec" $ do
    -- prop "edges half the size of the darts" $
    --   \(gr :: PlanarGraph MyWorld () () ()) ->
    --     (2 * lengthOf edges gr) `shouldBe` lengthOf darts gr

    it "outNeighboursOf" $ do
      let theVertices = (\u -> (u,) <$> toListOf (outNeighboursOf u.asIndex) myGraph')
                         <$> toListOf (vertices.asIndex) myGraph'
      theVertices `shouldBe` ((\(u,_,adj) -> (\(v,_) -> (u,v)) <$> F.toList adj)
                               <$> F.toList testEdges)


    it "dart endpoints ok" $ do
      forM_ (myDiGraph'^..darts.asIndex) $ \d ->
         (myDiGraph'^?!dartAt d) `shouldBe` (let (VertexId u,VertexId v) = endPoints myDiGraph' d
                                             in Text.pack $ "edge" <> show (u,v)
                                            )

    it "outgoingDarts correct" $ do
      let theVertices = (\u -> (u, snd <$> toListOf (outgoingDartsOf u.withIndex) myDiGraph')
                        ) <$> toListOf (vertices.asIndex) myDiGraph'
      theVertices `shouldBe` ((\(u,_,adj) -> (u, snd <$> F.toList adj))
                               <$> F.toList testEdges)

    it "edges half the size of the darts" $
      let gr :: CPlanarGraph PlanarGraph.Primal TestG Int Text.Text ()
          gr = fromAdjacencyLists testEdges
      in (2 * lengthOf edges gr) `shouldBe` lengthOf darts gr

    sameGraphs "testEdges" (fromAdjacencyLists testEdges)
                           (fromAdjacencyListsOld $ simplify testEdges)
    prop "quickheck Dart:  (toEnum (fromEnum d)) = d" $
      \(d :: DartId TestG) -> toEnum (fromEnum d) `shouldBe` d
    prop "quickheck Dart: fromEnum (toEnum i) = i" $
      \(NonNegative i) -> fromEnum ((toEnum i) :: DartId TestG) `shouldBe` i
    it "encode yaml test" $ do
      b <- File.readFile' [osp|data/PlanarGraph/myGraph.yaml|]
      encodeYAML myDiGraph' `shouldBe` b
    it "decode yaml test" $ do
      (decodeYAMLFile [osp|data/PlanarGraph/myGraph.yaml|])
      `shouldReturn`
      (Right myDiGraph')


-- test :: IO (PlanarGraph TestG PlanarGraph.Primal Int Text.Text ())
-- test = buildGraph . adjacencies <$> testGr


-- testGr :: IO (Gr (Vtx Int Text.Text) (Face ()))
-- testGr = do Right x <- decodeYAMLFile [osp|data/PlanarGraph/myGraph.yaml|]
--             pure x

myDiGraph' :: CPlanarGraph PlanarGraph.Primal TestG Int Text.Text ()
myDiGraph' = diGraphFromAdjacencyLists testEdges

myGraph' :: CPlanarGraph PlanarGraph.Primal TestG Int Text.Text ()
myGraph' = fromAdjacencyLists testEdges

testEdges :: NonEmpty (Vertex, Int, NonEmpty (Vertex, Text.Text ))
testEdges = fmap (\(i,vs) -> (VertexId i, i,
                              fmap (\j -> (VertexId j, Text.pack $ "edge" <> show (i,j))) vs))
          $ NonEmpty.fromList
            [ (0, NonEmpty.fromList [1])           -- 1
            , (1, NonEmpty.fromList [0,2,4])       -- 3
            , (2, NonEmpty.fromList [1,3,4])       -- 3
            , (3, NonEmpty.fromList [2,5])         -- 2
            , (4, NonEmpty.fromList [1,2,5])       -- 3
            , (5, NonEmpty.fromList [3,4])         -- 2
            ]

simplify :: NonEmpty (Vertex, Int, NonEmpty (Vertex, Text.Text ))
         -> [(Vertex, [Vertex])]
simplify = F.toList . fmap (\(u,_,adjs) -> (u, F.toList $ fmap fst adjs))

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
                        -> CPlanarGraph PlanarGraph.Primal s () () ()
fromAdjacencyListsOld adjM = cPlanarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ perm
    perm = NonEmpty.fromList
         . fmap NonEmpty.fromList
         . view (_3) . foldr toOrbit (STR mempty 0 mempty) $ adjM


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
