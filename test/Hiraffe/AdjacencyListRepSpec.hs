module Hiraffe.AdjacencyListRepSpec (spec) where

import Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import Hiraffe.AdjacencyListRep
import Hiraffe.Graph
import Test.Hspec
--------------------------------------------------------------------------------


spec :: Spec
spec = describe "adjacencylist representation tests" $ do
         it "edges" $
           testG^..edges.withIndex `shouldBe` [ ((0,1), "01")
                                              , ((0,2), "02")
                                              , ((1,2), "12")
                                              ]

         it "darts" $
           testG^..darts.withIndex `shouldBe` [ ((0,1), "01")
                                              , ((0,2), "02")
                                              , ((1,0), "10")
                                              , ((1,2), "12")
                                              , ((2,0), "20")
                                              , ((2,1), "21")
                                              ]





-- | some test graph
testG :: Graph Int String
testG = fromAdjacencyLists . NonEmpty.fromList $
                           [ (0, 0, NonEmpty.fromList [ (1, "01"), (2, "02") ])
                           , (1, 1, NonEmpty.fromList [ (0, "10"), (2, "12") ])
                           , (2, 2, NonEmpty.fromList [ (1, "21"), (0, "20") ])
                           ]
