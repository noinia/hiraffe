module Hiraffe.AdjacencyListRepSpec (spec) where

import Control.Lens
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
testG = fromAdjacencyLists [ (0, 0, [ (1, "01"), (2, "02") ])
                           , (1, 1, [ (0, "10"), (2, "12") ])
                           , (2, 2, [ (1, "21"), (0, "20") ])
                           ]
