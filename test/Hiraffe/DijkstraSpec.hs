module Hiraffe.DijkstraSpec where

import Control.Lens
import HGeometry.Unbounded
import Hiraffe.Graph.Class
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Hiraffe.ShortestPathSpec (gridGraph, edgeLengths, l1Distance)
import Hiraffe.ShortestPath.Dijkstra qualified as Dijkstra
import Data.Semigroup
import Data.Coerce

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Dijkstra" $ do
         it "manual test1" $
           shortestPaths' 's' testGraph
           `shouldBe`
           [('s',ValT Sum {getSum = 0})
           ,('a',ValT Sum {getSum = 5})
           ,('b',ValT Sum {getSum = 6})
           ,('c',ValT Sum {getSum = 10})
           ]
         modifyMaxSize (const 15) $ do
           prop "grid graph distances correct" $
             \(NonNegative nc) (NonNegative nr) ->
               let gr   = gridGraph (nc,nr)
                   adjs = [ (u, gr^..outNeighboursOf u.asIndex)
                          | u <- gr^..vertices.asIndex
                          ]
                   edgeLengths'     :: Int -> Int -> Top (Sum Int)
                   edgeLengths' u v = coerce @(Top Int) $ edgeLengths gr u v
                   res  = (\s -> (s, Dijkstra.shortestPaths edgeLengths' s adjs))
                          <$> gr^..vertices.asIndex
               in conjoin $ map (\(s,dists) ->
                                   conjoin [counterexample (show (gr^?!vertexAt s.withIndex
                                                                 ,gr^?!vertexAt v.withIndex
                                                                 )) $
                                            coerce dist === ValT (l1Distance gr s v)
                                           | (v, dist) <- dists
                                           ]
                                ) res

type WeightedGraph v r = [(v,[(v,r)])]


shortestPaths'         :: (Ord v, Monoid r, Ord r) => v -> WeightedGraph v r -> [(v, Top r)]
shortestPaths' s graph =  Dijkstra.shortestPaths (weightF graph) s (dropWeights graph)

-- | Get the edge length/weight from u to v
weightF           :: Eq v => WeightedGraph v r -> v -> v -> Top r
weightF graph u v = review _TopMaybe $ lookup u graph >>= lookup v

-- | return just an adjacency list representation
dropWeights :: WeightedGraph v r -> [(v,[v])]
dropWeights = map (\(v,vs) -> (v,map fst vs))

testGraph :: WeightedGraph Char (Sum Int)
testGraph = coerce $
            [ ('s', [('a',5 :: Int),('b',10)])
            , ('a', [('s',5),('b',1),('c',6)])
            , ('b', [('s',10),('a',1),('c',4)])
            , ('c', [('a',6),('b',4)])
            ]
