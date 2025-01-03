module Hiraffe.PlanarGraph.EdgeOracleSpec where

import           Control.Arrow
import qualified Data.Set as S
import           Hiraffe.PlanarGraph (VertexIdIn(..), World(..))
import           Hiraffe.PlanarGraph.EdgeOracle
import           Test.Hspec

--------------------------------------------------------------------------------

data TestG

type Vertex = VertexIdIn Primal TestG

testEdges :: [(Vertex,[Vertex])]
testEdges = map (\(i,vs) -> (VertexId i, map VertexId vs))
            [ (0, [1])
            , (1, [0,1,2,4])
            , (2, [1,3,4])
            , (3, [2,5])
            , (4, [1,2,5])
            , (5, [3,4])
            ]

buildEdgeOracle'  :: [(Vertex,[Vertex])] -> EdgeOracle Primal TestG ()
buildEdgeOracle' = buildEdgeOracle . map (second $ fmap (,()) )

-- | Flattens an adjacencylist representation into a set of edges
flattenEdges :: [(t, [a])] -> [(t, a)]
flattenEdges = concatMap (\(i,vs) -> map (i,) vs)

-- | Given a set of edges, generates all non-edges, i.e. all pairs of vertices
-- that do not form an edge
nonEdges    :: [(VertexIdIn w s , [VertexIdIn w s ])] -> [(VertexIdIn w s , VertexIdIn w s )]
nonEdges es = flattenEdges . map (second $ f . S.fromList) $ es
  where
    f vs  = filter (`S.notMember` vs) allVs
    allVs = map fst es

-- | Retains only the edges in the graph
hasEdges         :: EdgeOracle w s a -> [(VertexIdIn w s , VertexIdIn w s )]
                 -> [(VertexIdIn w s , VertexIdIn w s )]
oracle `hasEdges` es = filter (\(u,v) -> hasEdge u v oracle) es


-- | Tests all edges es
edgeOracleSpec      :: String -> [(Vertex, [Vertex])]  -> Spec
edgeOracleSpec s es = do
    let oracle = buildEdgeOracle' es
    describe ("EdgeOracle on " <> s) $ do
      it "test postitive edges" $
          (oracle `hasEdges` flattenEdges es) `shouldBe` flattenEdges es
      it "test negative edges " $
          (oracle `hasEdges` nonEdges es) `shouldBe` []

      -- it "test maximum adjacency-list lengths" $
      --     (filter (\v -> length v > 6) . _unEdgeOracle $ oracle) `shouldBe` []

spec :: Spec
spec = do
         edgeOracleSpec "testEdges" testEdges
