module Hiraffe.DijkstraSpec where

import Control.Lens
import Data.Array qualified as Array
import Data.List.NonEmpty qualified as NonEmpty
import HGeometry.Unbounded
import Hiraffe.AdjacencyListRep.Map
import Hiraffe.Graph.Class
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Witherable
import Hiraffe.ShortestPathSpec (gridGraph, l1Distance, l1Distance')
import Hiraffe.ShortestPath.Dijkstra qualified as Dijkstra

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Dijkstra" $ do
         it "manual test1" $
           shortestPaths' 's' testGraph
           `shouldBe`
           [] -- this is nonsense


type WeightedGraph v r = [(v,[(v,r)])]


shortestPaths'         :: (Ord v, Monoid r, Ord r) => v -> WeightedGraph v r -> [(v, Top r)]
shortestPaths' s graph =  shortestPaths (weightF graph) s (dropWeights graph)

weightF           :: Eq v => WeightedGraph v r -> v -> v -> Top r
weightF graph u v = review _TopMaybe $ lookup u graph >>= lookup v

dropWeights :: WeightedGraph v r -> [(v,[v])]
dropWeights = map (\(v,vs) -> (v,map fst vs))

testGraph :: WeightedGraph Char (Sum Int)
testGraph = coerce $
            [ ('s', [('a',5 :: Int),('b',10)])
            , ('a', [('s',5),('b',1),('c',6)])
            , ('b', [('s',10),('a',1),('c',4)])
            , ('c', [('a',6),('b',4)])
            ]
