module Hiraffe.DualSpec where


import           Control.Lens hiding ((.=))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Tuple (swap)
import qualified Data.Vector.NonEmpty as V
import           Hiraffe.PlanarGraph
import           Test.Hspec
import qualified Hiraffe.PlanarGraph.Dart as Dart

--------------------------------------------------------------------------------

data MyWorld

spec :: Spec
spec = describe "PlanarGraph dual graph spec" $ do
         it "faces" $
           myGraph^..faces.withIndex `shouldBe` [ (FaceId $ VertexId 0,"f_3")
                                                , (FaceId $ VertexId 1,"f_infty")
                                                , (FaceId $ VertexId 2,"f_1")
                                                , (FaceId $ VertexId 3,"f_2")
                                                ]
         it "leftFace tests" $ do
           (myGraph^.leftFaceOf (dart "b+").withIndex) `shouldBe` (FaceId $ VertexId 1,"f_infty")
           (myGraph^.leftFaceOf (dart "a+").withIndex) `shouldBe` (FaceId $ VertexId 0,"f_3")
           (myGraph^.leftFaceOf (dart "b-").withIndex) `shouldBe` (FaceId $ VertexId 2,"f_1")
         it "rightFace tests" $ do
           (myGraph^.rightFaceOf (dart "b+").withIndex) `shouldBe` (FaceId $ VertexId 2,"f_1")
           (myGraph^.rightFaceOf (dart "a+").withIndex) `shouldBe` (FaceId $ VertexId 1,"f_infty")
           (myGraph^.rightFaceOf (dart "b-").withIndex) `shouldBe` (FaceId $ VertexId 1,"f_infty")
           (myGraph^.rightFaceOf (dart "d+").withIndex) `shouldBe` (FaceId $ VertexId 3,"f_2")
         it "nextDartOf tests" $ do
           (myGraph^.nextDartOf (dart "b+")) `shouldBe` "e-"
           (myGraph^.nextDartOf (dart "a+")) `shouldBe` "a+"
           (myGraph^.nextDartOf (dart "a-")) `shouldBe` "b+"
           (myGraph^.nextDartOf (dart "b-")) `shouldBe` "c+"
           (myGraph^.nextDartOf (dart "d+")) `shouldBe` "b-"
           (myGraph^.nextDartOf (dart "d-")) `shouldBe` "e+"
           (myGraph^.nextDartOf (dart "c+")) `shouldBe` "d+"
         it "prevDartOf tests" $ do
           (myGraph^.prevDartOf (dart "b+")) `shouldBe` "a-"
           (myGraph^.prevDartOf (dart "d+")) `shouldBe` "c+"
           (myGraph^.prevDartOf (dart "d-")) `shouldBe` "g-"
           (myGraph^.prevDartOf (dart "c+")) `shouldBe` "b-"
           (myGraph^.prevDartOf (dart "c-")) `shouldBe` "e-"
         it "boundaryTests" $ do
           (myGraph^..boundaryDartsOf (FaceId $ VertexId 2)) `shouldBe` ["c+","d+","b-"]
           (myGraph^..boundaryDartsOf (FaceId $ VertexId 1)) `shouldBe` ["a-","b+","e-","c-"]
         it "boundaryDartOf" $ do
           (myGraph^.boundaryDartOf (FaceId $ VertexId 2)) `shouldBe` "c+"
           (myGraph^.boundaryDartOf (FaceId $ VertexId 1)) `shouldBe` "a-"


dart   :: String -> Dart.Dart MyWorld
dart s = fromMaybe (error "dart not found") $ lookup s ds
  where
    ds = map swap $ myGraph^..darts.withIndex

myGraph :: CPlanarGraph MyWorld Primal String String String
myGraph = planarGraph adjacencies
                    & vertexData .~ V.unsafeFromList ["u","v","w","x"]
                    & faceData   .~ V.unsafeFromList ["f_3", "f_infty","f_1","f_2"]
  where
    (aA:aB:aC:aD:aE:aG:_) = take 6 [Dart.Arc 0..]
    adjacencies = NonEmpty.fromList . fmap NonEmpty.fromList $
                          [ [ (Dart.Dart aA Negative, "a-")
                            , (Dart.Dart aC Positive, "c+")
                            , (Dart.Dart aB Positive, "b+")
                            , (Dart.Dart aA Positive, "a+")
                            ]
                          , [ (Dart.Dart aE Negative, "e-")
                            , (Dart.Dart aB Negative, "b-")
                            , (Dart.Dart aD Negative, "d-")
                            , (Dart.Dart aG Positive, "g+")
                            ]
                          , [ (Dart.Dart aE Positive, "e+")
                            , (Dart.Dart aD Positive, "d+")
                            , (Dart.Dart aC Negative, "c-")
                            ]
                          , [ (Dart.Dart aG Negative, "g-")
                            ]
                          ]

-- showWithData     :: HasDataOf s i => s -> i -> (i, DataOf s i)
-- showWithData g i = (i, g^.dataOf i)
