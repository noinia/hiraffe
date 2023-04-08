{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Hiraffe.AdjacencyListRep
  ( Graph
  ) where

import           Control.Lens
import           Data.Bifoldable
import           Data.Bitraversable
import qualified Data.Foldable as F
import           Data.Functor.Classes
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Hiraffe.Graph

--------------------------------------------------------------------------------

-- | stores the vertex data. the order
data VertexData f v e = VertexData { _vData      :: !v
                                   , _neighMap   :: IntMap.IntMap e
                                   , _neighOrder :: f Int
                                   } deriving (Generic,Functor,Foldable,Traversable)
makeLenses ''VertexData


instance (Show1 f, Show v, Show e) => Show (VertexData f v e) where
  showsPrec d (VertexData v ns nOrd) =
    showParen
          (d >= 11)
          ((.)
             (showString "VertexData ")
             ((.)
                (showsPrec 11 v)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 ns)
                      ((.)
                         showSpace (showsPrec1 11 nOrd))))))
    where
      showSpace = (' ' :)
      -- horrible instance more or less generated by GHC

instance (Eq1 f, Eq v, Eq e) => Eq (VertexData f v e) where
  (VertexData v ns nOrd) == (VertexData v' ns' nOrd') =
    v == v' && liftEq (==) nOrd nOrd' && liftEq (==) ns ns'

instance (Ord1 f, Ord v, Ord e) => Ord (VertexData f v e) where
  (VertexData v ns nOrd) `compare` (VertexData v' ns' nOrd') =
    v `compare` v' <> liftCompare compare nOrd nOrd' <> liftCompare compare ns ns'

instance Bifunctor (VertexData f) where
  bimap f g (VertexData v ns nOrd) = VertexData (f v) (fmap g ns) nOrd
instance Bifoldable (VertexData f) where
  bifoldMap f g (VertexData v ns _) = f v <> foldMap g ns

instance Bitraversable (VertexData f) where
  bitraverse f g (VertexData v ns nOrd) =
    (\v' ns' -> VertexData v' ns' nOrd) <$> f v <*> traverse g ns

-- | A graph in which the vertices themselves are indices
newtype GGraph f v e = Graph (IntMap.IntMap (VertexData f v e))
                    deriving (Show,Eq,Generic,Functor,Foldable,Traversable)
type Graph = GGraph Seq.Seq


_GraphIntMap :: Iso (GGraph f v e)                     (GGraph f' v' e')
                    (IntMap.IntMap (VertexData f v e)) (IntMap.IntMap (VertexData f' v' e'))
_GraphIntMap = iso (\(Graph m) -> m) Graph

instance HasVertices' (GGraph f v e) where
  type Vertex   (GGraph f v e) = v
  type VertexIx (GGraph f v e) = Int

  -- | \(O(\log n)\)
  vertexAt i = vertexDataOf i <. vData
  numVertices (Graph m) = IntMap.size m

vertexDataOf  :: VertexIx (GGraph f v e)
              -> IndexedTraversal' (VertexIx (GGraph f v e))
                                   (GGraph f v e)
                                   (VertexData f v e)
vertexDataOf i = _GraphIntMap .> iix i


instance HasVertices (GGraph f v e) (GGraph f v' e) where
  -- | running time: \(O(n)\).
  vertices = conjoined traverse' (itraverse' . indexed)
    where
      traverse'  :: Applicative g => (v -> g v') -> GGraph f v e -> g (GGraph f v' e)
      traverse'  = _GraphIntMap.traversed.vData
      itraverse'             :: Applicative g
                             => (Int -> v -> g v') -> GGraph f v e -> g (GGraph f v' e)
      itraverse' f (Graph m) =
        Graph <$> IntMap.traverseWithKey (\i vd -> vd&vData %%~ f i) m


instance HasEdges' (GGraph f v e) where
  type Edge   (GGraph f v e) = e
  type EdgeIx (GGraph f v e) = (VertexIx (GGraph f v e), VertexIx (GGraph f v e))
                          -- lexicographical index

  -- | running time: \(O(\log m)\)
  edgeAt (u,v) = vertexDataOf u <.> neighMap .> iix v

  -- | running time: O(n)
  --
  -- TODO: also, this reports all *directed* edges; so I guess for undirected graphs
  -- this returns twice the number of edges
  numEdges    (Graph m) = getSum . foldMap (Sum . lengthOf neighMap) $ m


instance HasEdges (GGraph f v e) (GGraph f v e') where
  -- | running time: \(O(m)\)
  edges pefe' (Graph m) = Graph <$> IntMap.traverseWithKey f m
    where
      f u vd = vd&neighMap.itraversed %%@~ \v e -> indexed pefe' (u,v) e


class HasFromList t where
  fromList :: [a] -> t a

instance HasFromList [] where
  fromList = id
instance HasFromList Seq.Seq where
  fromList = Seq.fromList
instance Monoid c => HasFromList (Const c) where
  fromList _ = Const mempty


instance (Ord v, HasFromList f) => Graph_ (GGraph f v e) where
  fromAdjacencyLists = fromMap . Map.fromList . zipWith f [0..] . F.toList
    where
      f i (v,adjs) = (v, (i, F.toList adjs))

      fromMap   :: Map.Map v (Int, [ (v, e) ] ) -> GGraph f v e
      fromMap m = Graph . IntMap.fromList . Map.elems . Map.mapWithKey g $ m
        where
          g v (i, adjs) = let adjs' = mapMaybe (\(u,e) -> (, e) <$> m ^? ix u ._1) adjs
                          in (i, VertexData v (IntMap.fromList adjs') (fromList $ map fst adjs'))
  -- TODO: make a version specific for when v is int, we can then use an intmap directly
  -- that should be faster

  neighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (GGraph f v e) v
      asFold  = folding  $ \g -> g^..incidentEdges' u.asIndex.to (\v -> g^?! vertexAt v)
      asIFold = ifolding $ \g -> g^..incidentEdges' u.asIndex.to (\v -> (v, g^?! vertexAt v))


  incidentEdges u = reindexed (u,) (incidentEdges' u)



-- | A traversal of the edges incident to vertex u as an intexed by the other vertex
incidentEdges'  :: VertexIx (GGraph f v e)
                -> IndexedTraversal' (VertexIx (GGraph f v e)) (GGraph f v e) e
-- incidentEdges'  :: (Indexable (VertexIx (GGraph f v e)) p, Applicative g)
--                 => VertexIx (GGraph f v e) -> p e (g e) -> GGraph f v e -> g (GGraph f v e)
incidentEdges' u = vertexDataOf u.neighMap.itraversed


-- | some test graph
test :: Graph Int String
test = fromAdjacencyLists [ (0, [ (1, "01"), (2, "02") ] )
                          , (1, [ (0, "10"), (2, "12") ] )
                          , (2, [ (1, "21"), (0, "20") ])
                          ]
