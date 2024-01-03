module Hiraffe.BFSSpec (spec) where

import Data.Graph (Graph,buildG)
import Data.Tree
import Hiraffe.BFS
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = it "BFS test" $
         bfs testGr 0 `shouldBe` answer

testGr :: Graph
testGr = buildG (0,8) [ (i,j) | (i,ns) <- es, j <- ns ]
  where
    es = [ (0, [1,3, 6])
         , (1, [2]     )
         , (2, [3, 5]  )
         , (3, [2 ]    )
         , (4, []      )
         , (5, [4, 5]  )
         , (6, [7]     )
         , (7, [8,4]   )
         , (8, [2]     )
         ]

answer :: Tree Int
answer = Node {rootLabel = 0,
               subForest = [Node {rootLabel = 1
                                 , subForest = [Node {rootLabel = 2
                                                     , subForest = [Node {rootLabel = 5
                                                                         , subForest = []}]}]},Node {rootLabel = 3, subForest = []},Node {rootLabel = 6, subForest = [Node {rootLabel = 7, subForest = [Node {rootLabel = 8, subForest = []},Node {rootLabel = 4, subForest = []}]}]}]}
