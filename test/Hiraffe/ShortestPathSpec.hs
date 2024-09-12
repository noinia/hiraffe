module Hiraffe.ShortestPathSpec(spec) where


import qualified Data.Foldable as F
import           Data.Graph (Graph,buildG)
import qualified Data.Map as Map
import           Data.Tree
import           Hiraffe.BFS
import           Hiraffe.Tree
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "All pair shortest path test (Floyd Warshall)" $ do
         pure ()


         -- prop "grid graph distances correct" $
         --   \n ->
         --     let allPairsShortestPathsWith
         --   bfs testGr 0 `shouldBe` answer
