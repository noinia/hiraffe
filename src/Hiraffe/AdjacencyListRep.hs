{-# LANGUAGE TemplateHaskell #-}
module Hiraffe.AdjacencyListRep
  ( Graph
  ) where


import           Control.Lens
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Sequence as Seq
import           GHC.Generics
import           Hiraffe.Graph


--------------------------------------------------------------------------------

data Adj e = Adj { vId    :: {-# UNPACK #-}!Int -- = VertexIx v
                 , eData :: !e
                 } deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)
makeLenses ''Adj


data VertexData v e = VertexData { _vData :: !v
                                 , _neighs :: Seq.Seq (Adj e)
                                 } deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)
makeLenses ''VertexData

instance Bifunctor VertexData where
  bimap = bimapDefault
instance Bifoldable VertexData where
  bifoldMap = bifoldMapDefault
instance Bitraversable VertexData where
  bitraverse f g (VertexData v ns) = VertexData <$> f v <*> traverse (traverse g) ns

-- | A graph in which the vertices themselves are indices
newtype Graph v e = Graph (IntMap.IntMap (VertexData v e))
                  deriving (Show,Eq,Generic,Functor,Foldable,Traversable)

instance HasVertices' (Graph v e) where
  type Vertex   (Graph v e) = v
  type VertexIx (Graph v e) = Int

  vertexAt i = vertexDataOf i <. vData

vertexDataOf                  :: VertexIx v
                              -> IndexedTraversal' (VertexIx (Graph v e))
                                                   (Graph v e)
                                                   (VertexData v e)
vertexDataOf i pafb (Graph m) = Graph <$> iix i pafb m

instance HasVertices (Graph v e) (Graph v' e) where
  -- this is going to be expensive....
  vertices pvfv' (Graph m) = Graph <$> IntMap.traverseWithKey f m
    where
      f i vd = vd&vData %%~ indexed pvfv' i

instance HasEdges' (Graph v e) where
  type Edge   (Graph v e) = e
  type EdgeIx (Graph v e) = (Int,Int) -- = (VertexIx v,VertexIx v) -- lexicographical index

  edgeAt (u,v) = vertexDataOf u <.> edge v
    where
      edge v pefe' vd = undefined -- vd&neighs.i
      -- TODO: store neighs differently, since this way we cannot efficiently find v.

instance HasEdges (Graph v e) (Graph v e') where
  edges pefe' (Graph m) = Graph <$> IntMap.traverseWithKey f m
    where
      f u vd = vd&neighs.traverse %%~ \(Adj v e) -> Adj v <$> indexed pefe' (u,v) e


instance Ord v => Graph_ (Graph v e) where
  fromAdjacencyLists = undefined

    -- Graph . foldMap (\(v, adjs) -> Map.singleton v (f adjs))
    -- where
    --   f = foldMap Seq.singleton



  numVertices (Graph m) = IntMap.size m
  numEdges    (Graph m) = getSum . foldMap (Sum . undefined) $ m

  neighboursOf u = undefined

  incidentEdges u = undefined
