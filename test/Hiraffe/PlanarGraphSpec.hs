{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Hiraffe.PlanarGraphSpec where

import           Control.Lens -- (view,_3,ifoldMapOf,(^..), asIndex, lengthOf, headOf)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as SM
import qualified Data.Set as S
import qualified Data.Text as Text
import           Data.YAML
import           HGeometry.Combinatorial.Util
import           HGeometry.Permutation (toCycleRep)
import           HGeometry.YAML
import           Hiraffe.Instances ()
-- import           Hiraffe.PlanarGraph ( CPlanarGraph, VertexId, DartId, VertexIdIn(..)
--                                      , neighboursOf, planarGraph', vertices
--                                      , darts, edges
--                                      )
import qualified Hiraffe.PlanarGraph as PlanarGraph
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.IO
import           Hiraffe.PlanarGraph
import qualified System.File.OsPath as File
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------
data TestG

type Vertex = VertexId TestG

spec :: Spec
spec = describe "PlanarGraph spec" $ do
  pure ()



    -- -- prop "edges half the size of the darts" $
    -- --   \(gr :: PlanarGraph MyWorld () () ()) ->
    -- --     (2 * lengthOf edges gr) `shouldBe` lengthOf darts gr
    -- it "edges half the size of the darts" $
    --   let gr :: CPlanarGraph PlanarGraph.Primal TestG Int Text.Text ()
    --       gr = fromAdjacencyLists testEdges
    --   in (2 * lengthOf edges gr) `shouldBe` lengthOf darts gr

    -- sameGraphs "testEdges" (fromAdjacencyLists testEdges) (fromAdjacencyListsOld $ simplify testEdges)
    -- prop "quickheck Dart:  (toEnum (fromEnum d)) = d" $
    --   \(d :: DartId TestG) -> toEnum (fromEnum d) `shouldBe` d
    -- prop "quickheck Dart: fromEnum (toEnum i) = i" $
    --   \(NonNegative i) -> fromEnum ((toEnum i) :: DartId TestG) `shouldBe` i
    -- it "encode yaml test" $ do
    --   b <- File.readFile' [osp|data/PlanarGraph/myGraph.yaml|]
    --   encodeYAML (fromAdjacencyLists testEdges) `shouldBe` b
    -- it "decode yaml test" $ do
    --   (decodeYAMLFile [osp|data/PlanarGraph/myGraph.yaml|])
    --   `shouldReturn`
    --   (Right $ fromAdjacencyLists testEdges)


-- test :: IO (PlanarGraph TestG PlanarGraph.Primal Int Text.Text ())
-- test = buildGraph . adjacencies <$> testGr


-- testGr :: IO (Gr (Vtx Int Text.Text) (Face ()))
-- testGr = do Right x <- decodeYAMLFile [osp|data/PlanarGraph/myGraph.yaml|]
--             pure x

myGraph :: PlanarGraph TestG PlanarGraph.Primal Int Text.Text ()
myGraph = fromConnected' myCGraph $ headOf darts myCGraph


  -- fromAdjacencyLists testEdges

myCGraph :: CPlanarGraph TestG PlanarGraph.Primal Int Text.Text ()
myCGraph = fromAdjacencyLists testEdges


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
