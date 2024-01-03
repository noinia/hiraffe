module Hiraffe.BFSSpec (spec) where

import qualified Data.Foldable as F
import           Data.Graph (Graph,buildG)
import qualified Data.Map as Map
import           Data.Tree
import           Hiraffe.BFS
import           Hiraffe.Tree
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "BFS test" $ do
         it "BFS tree" $
           bfs testGr 0 `shouldBe` answer
         it "distances" $
           let dists = Map.fromList . F.toList . labelWithDistances $ bfs testGr 0
           in dists `shouldBe` distances

labelWithDistances :: Tree a -> Tree (a, Int)
labelWithDistances = labelPrefixWith 0 (const succ)

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

distances :: Map.Map Int Int
distances = Map.fromList [ (0,0)
                         , (1,1)
                         , (2,2)
                         , (3,1)
                         , (4,3)
                         , (5,3)
                         , (6,1)
                         , (7,2)
                         , (8,3)
                         ]


answer :: Tree Int
answer = Node 0 [ Node 6 [Node 7 [ Node 4 []
                                 , Node 8 []
                                 ]
                         ]
                , Node 3 [ Node 2 [ Node 5 [] ]
                         ]
                , Node 1  []
                ]
