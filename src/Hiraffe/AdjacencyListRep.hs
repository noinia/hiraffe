module Hiraffe.AdjacencyListRep
  ( Graph
  ) where


import           Control.Lens
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Hiraffe.Graph


--------------------------------------------------------------------------------

newtype Graph v e = Graph (Map.Map v (Seq.Seq (v,e)))
                  deriving (Show,Eq)






instance HasVertices' (Graph v e) where
  type Vertex   (Graph v e) = v
  type VertexIx (Graph v e) = Int

  vertexAt i pafb (Graph m) = undefined -- case Map.elemAt i m of
                                -- Nothing -> pure
    -- where
    --   mkv =

    -- pafb $
instance HasVertices (Graph v e) (Graph v' e) where
  vertices = undefined

instance HasEdges' (Graph v e) where
  type Edge   (Graph v e) = e
  type EdgeIx (Graph v e) = (Int,Int) -- lexicographical index


instance HasEdges (Graph v e) (Graph v e') where



instance Ord v => Graph_ (Graph v e) where
  fromAdjacencyLists = Graph . foldMap (\(v, adjs) -> Map.singleton v (f adjs))
    where
      f = foldMap Seq.singleton



  numVertices (Graph m) = Map.size m
  numEdges    (Graph m) = getSum . foldMap (Sum . length) $ m

  neighboursOf u = undefined

  incidentEdges u = undefined
