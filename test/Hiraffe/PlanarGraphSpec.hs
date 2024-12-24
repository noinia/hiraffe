{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Hiraffe.PlanarGraphSpec where

import           Control.Lens
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
import qualified Hiraffe.PlanarGraph as PlanarGraph
import qualified Hiraffe.PlanarGraph.Dart as Dart
-- import           Hiraffe.PlanarGraph.IO
import           Hiraffe.PlanarGraph
import qualified System.File.OsPath as File
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------
data TestG

type Vertex' = VertexId TestG

spec :: Spec
spec = describe "PlanarGraph spec" $ do

  it "positive and negative dart same data" $ do
    forM_ (myGraph^..darts.withIndex) $ \(d,x) ->
      x `shouldBe` (myGraph^?!dartAt (myGraph^.twinOf d))


  it "outNeighboursOf" $ do
    let theVertices = (\u -> (u,) <$> toListOf (outNeighboursOf u.asIndex) myGraph)
                       <$> toListOf (vertices.asIndex) myGraph
    theVertices `shouldBe` ((\(u,_,adj) -> (\(v,_) -> (u,v)) <$> F.toList adj)
                             <$> F.toList testEdges)

  -- it "darts same data" $
  --   forM_ (myDiGraph^..darts) $ \y@(d,x)
  --     y `shouldBe`
  it "dart endpoints ok 0" $ do
      forM_ (myDiGraph^..darts.withIndex) $ \(d,dData) ->
        dData `shouldBe` (let (VertexId u,VertexId v) = endPoints myDiGraph d
                          in Text.pack $ "edge" <> show (u,v)
                         )


  it "dart endpoints ok 1" $ do
      forM_ (myDiGraph^..darts.asIndex) $ \d ->
         (myDiGraph^?!dartAt d) `shouldBe` (let (VertexId u,VertexId v) = endPoints myDiGraph d
                                             in Text.pack $ "edge" <> show (u,v)
                                            )

  it "dart endpoints ok" $ do
    let theVertices = (\(u,x) -> ( u
                                 , x
                                 , (\(d,v) -> (v, myGraph^?!dartAt d))
                                     <$> toListOf (outNeighboursOfByDart u . asIndex) myDiGraph
                                 )
                      ) <$> toListOf (vertices.withIndex) myDiGraph
    theVertices `shouldBe` ((\(u,x,adj) -> (u,x,F.toList adj)) <$> F.toList testEdges)


  it "edgesOk" $
    (myGraph^..edges) `shouldBe` (myCGraph^..edges)
  it "neighboursOk" $ do
    let u = myGraph^?!vertices.asIndex -- take the first vertex
    myGraph^..neighboursOfByEdge u `shouldBe` myCGraph^..neighboursOfByEdge u
  it "neighboursOfByEdge" $ do
    let (_:(u,x):_) = myGraph^..vertices.withIndex -- take the second vertex
    show (x, myGraph^..neighboursOfByEdge u.withIndex) `shouldBe`
      ""
  -- it "neighboursOfByEdge" $ do
  --   let (_:(u,x):_) = myGraph^..vertices.withIndex -- take the second vertex
  --   show (x, myGraph^..neighboursOfByEdge u.withIndex) `shouldBe`
  --     "(1,[((Dart (Arc 0) +1,VertexId 1),1),((Dart (Arc 1) +1,VertexId 2),2),((Dart (Arc 2) +1,VertexId 4),4)])"
  --     ""
-- , (1, NonEmpty.fromList [0,2,4])       -- 3
  it "outNeighboursOk" $ do
    let u = myGraph^?!vertices.asIndex -- take the first vertex
    show (myGraph^..neighboursOfByEdge u.withIndex) `shouldBe`
      "[((Dart (Arc 0) +1,VertexId 1),1)]"



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

myGraph :: PlanarGraph Primal TestG Int Text.Text ()
myGraph = fromConnected' myCGraph $ myCGraph^?!darts.asIndex

myDiGraph :: PlanarGraph Primal TestG Int Text.Text ()
myDiGraph = fromConnected' myCDiGraph $ myCGraph^?!darts.asIndex

  -- headOf (darts.asIndex) myCGraph


  -- fromAdjacencyLists testEdges

myCGraph :: CPlanarGraph Primal TestG Int Text.Text ()
myCGraph = fromAdjacencyLists testEdges

myCDiGraph :: CPlanarGraph PlanarGraph.Primal TestG Int Text.Text ()
myCDiGraph = diGraphFromAdjacencyLists testEdges


testEdges :: NonEmpty (Vertex', Int, NonEmpty (Vertex', Text.Text ))
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
