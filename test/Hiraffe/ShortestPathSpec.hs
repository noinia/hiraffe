module Hiraffe.ShortestPathSpec where


import           Control.Lens
import qualified Data.Array as Array
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Unbounded
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.Graph.Class
import           Hiraffe.ShortestPath.FloydWarshall
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
edgeLengths gr = \u v -> if u == v then ValT 0
                                   else let len = 1 <$ (gr^?dartAt (u,v))
                                        in len ^. re _TopMaybe

spec :: Spec
spec = describe "All pair shortest path test (Floyd Warshall)" $ do
         modifyMaxSize (const 15) $ do
           prop "grid graph distances correct" $
             \(NonNegative nc) (NonNegative nr) ->
               let gr  = gridGraph (nc,nr)
                       -- all edges have length 1
                   res = allPairsShortestPaths (edgeLengths gr) gr
               in conjoin $ map (\((u,v),dist) ->
                                   counterexample (show (gr^?!vertexAt u.withIndex
                                                        ,gr^?!vertexAt v.withIndex
                                                        )) $
                                     dist === ValT (l1Distance gr u v)
                                )
                                (Array.assocs res)
           prop "grid graph paths exist" $
             \(NonNegative nc) (NonNegative nr) ->
               let gr    = gridGraph (nc,nr)
                   table = allPairsShortestPathsWithNext (edgeLengths gr) gr


                   f     = \case
                     Top           -> []
                     ValT (_,u NonEmpty.:| path) -> zip (u:path) path
               in conjoin [ counterexample (show ((si,s),(ti,t),(u,v))) $
                            isn't _Nothing  $ gr^?dartAt (u,v)
                          | (si,s) <- gr^..vertices.withIndex
                          , (ti,t) <- gr^..vertices.withIndex
                          , (u,v)  <- f $ extractShortestPath table si ti
                          ]

         let gr    = gridGraph (4,6)
             table = allPairsShortestPathsWithNext (edgeLengths gr) gr
         prop "singleton paths" $
           conjoin $ map (\(i,v) -> verifyPath (gr, table) i i [v]) (gr^..vertices.withIndex)
         it "manual path test 1" $
           let (si,ti) = (0,1)
               s       = gr^?!vertexAt si
               t       = gr^?!vertexAt ti
           in verifyPath (gr,table) si ti ([s, t])
         it "manual path test" $
           let (si,ti) = (0,10)
               s       = gr^?!vertexAt si
               t       = gr^?!vertexAt ti
           in verifyPath (gr,table) si ti ([s, (0,1), t])

verifyPath (gr, table) si ti path = (s, t, lookupV <$> extractShortestPath table si ti)
                                    `shouldBe`
                                    (s, t, ValT ( l1Distance gr si ti, NonEmpty.fromList path))
  where
    lookupV = fmap (fmap (\v -> gr^?!vertexAt v))
    s  = gr^?!vertexAt si
    t  = gr^?!vertexAt ti
