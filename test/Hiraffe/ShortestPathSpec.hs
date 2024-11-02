module Hiraffe.ShortestPathSpec where


import           Control.Lens
import qualified Data.Array as Array
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Semigroup
import           Data.Tree
import           HGeometry.Unbounded
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.BFS
import           Hiraffe.Graph.Class
import           Hiraffe.ShortestPath.FloydWarshall
import           Hiraffe.Tree
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Witherable

--------------------------------------------------------------------------------

gridGraph         :: (Int,Int) -- ^ Max vertex; x,y
                  -> Graph Int (Int,Int) String
gridGraph (nc,nr) = fromAdjacencyLists (NonEmpty.fromList vs)
  where
    vs = [ ( f x y -- twould be nicer to just have the pair
           , (x,y)
           , imapMaybe mkEdge [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]
           )
         | x <- [0..nc], y <- [0..nr]
         ]
    f x y = (nc+1)* y + x

    mkEdge i (x,y)
      | x >= 0 && x <= nc && y >= 0 && y <= nr = Just (f x y, show i)
      | otherwise                              = Nothing




l1Distance' (x,y) (x',y') = abs (x'-x) + abs (y'-y)

l1Distance gr u v = l1Distance' (gr^?!vertexAt u) (gr^?!vertexAt v)



-- | Computes the length of an edge in the grid graph; if there is an edge it has length
-- 1.
edgeLengths gr = \u v -> if u == v then ValT (Sum 0)
                                   else let len = Sum 1 <$ (gr^?dartAt (u,v))
                                        in len ^. re _TopMaybe

spec :: Spec
spec = describe "All pair shortest path test (Floyd Warshall)" $ do
         modifyMaxSize (const 15) $
           prop "grid graph distances correct" $
             \(NonNegative nc) (NonNegative nr) ->
               let gr  = gridGraph (nc,nr)
                       -- all edges have length 1
                   res = allPairsShortestPathsWith (edgeLengths gr) gr
               in conjoin $ map (\((u,v),dist) ->
                                   counterexample (show (u -- gr^?!vertexAt u.withIndex
                                                        ,v) -- gr^?!vertexAt v.withIndex)
                                                  ) $
                                   counterexample ("expected" <> show
                                                   (ValT (Sum $ l1Distance gr u v))) $
                                     dist === ValT (Sum $ l1Distance gr u v))
                                (Array.assocs res)
