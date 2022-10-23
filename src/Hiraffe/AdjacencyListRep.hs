{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Hiraffe.AdjacencyListRep
  ( Graph
  ) where


import           Control.Lens
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Functor.Classes
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Sequence as Seq
import           GHC.Generics
import           Hiraffe.Graph


--------------------------------------------------------------------------------

data Adj e = Adj { vId    :: {-# UNPACK #-}!Int -- = VertexIx v
                 , eData :: !e
                 } deriving (Show,Read,Eq,Ord,Generic,Functor,Foldable,Traversable)
makeLenses ''Adj


data VertexData f v e = VertexData { _vData :: !v
                                   , _neighs :: f (Adj e)
                                   } deriving (Generic,Functor,Foldable,Traversable)
makeLenses ''VertexData


instance (Show1 f, Show v, Show e) => Show (VertexData f v e) where
  -- showsPrec (VertexData v ns) = "VertexData"

instance (Eq1 f, Eq v, Eq e) => Eq (VertexData f v e) where
  (VertexData v ns) == (VertexData v' ns') = v == v' && liftEq (==) ns ns'
instance (Ord1 f, Ord v, Ord e) => Ord (VertexData f v e) where
  (VertexData v ns) `compare` (VertexData v' ns') =
    v `compare` v' <> liftCompare compare ns ns'

instance Functor f => Bifunctor (VertexData f) where
  bimap f g (VertexData v ns) = VertexData (f v) (fmap (fmap g) ns)
instance Foldable f => Bifoldable (VertexData f) where
  bifoldMap f g (VertexData v ns) = f v <> foldMap (foldMap g) ns

instance Traversable f => Bitraversable (VertexData f) where
  bitraverse f g (VertexData v ns) = VertexData <$> f v <*> traverse (traverse g) ns

-- | A graph in which the vertices themselves are indices
newtype GGraph f v e = Graph (IntMap.IntMap (VertexData f v e))
                    deriving (Show,Eq,Generic,Functor,Foldable,Traversable)
type Graph = GGraph Seq.Seq

instance HasVertices' (GGraph f v e) where
  type Vertex   (GGraph f v e) = v
  type VertexIx (GGraph f v e) = Int

  vertexAt i = vertexDataOf i <. vData

vertexDataOf                  :: VertexIx (GGraph f v e)
                              -> IndexedTraversal' (VertexIx (GGraph f v e))
                                                   (GGraph f v e)
                                                   (VertexData f v e)
vertexDataOf i pafb (Graph m) = Graph <$> iix i pafb m

instance HasVertices (Graph v e) (Graph v' e) where
  -- this is going to be expensive....
  vertices pvfv' (Graph m) = Graph <$> IntMap.traverseWithKey f m
    where
      f i vd = vd&vData %%~ indexed pvfv' i

instance HasEdges' (Graph v e) where
  type Edge   (Graph v e) = e
  type EdgeIx (Graph v e) = (VertexIx (Graph v e), VertexIx (Graph v e))
                          -- lexicographical index

  edgeAt (u,v) = vertexDataOf u <.> edge v
    where
      edge            :: VertexIx (Graph v e)
                      -> IndexedTraversal' (VertexIx (Graph v e))
                                           (VertexData f v e)
                                           e
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
  numEdges    (Graph m) = getSum . foldMap (Sum . lengthOf neighs) $ m

  neighboursOf u = vertexDataOf u . neighs .> trav
    where
      trav = undefined

  incidentEdges u = undefined
