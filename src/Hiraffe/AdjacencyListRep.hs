{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.AdjacencyListRep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of graphs using adjacency lists.
--
--------------------------------------------------------------------------------
module Hiraffe.AdjacencyListRep
  ( Graph
  ) where

import           Control.Lens
import           Data.Bifoldable
import           Data.Bitraversable
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Classes
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Internal as IntMapInternal
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           HGeometry.Foldable.Util
import           Hiraffe.Graph.Class


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

-- | A graph in which the adjacency lists are stored using Data.Sequence
type Graph = GGraph Seq.Seq

-- | Iso for accessing the underling int-map representing the adjacencies
_GraphIntMap :: Iso (GGraph f v e)                     (GGraph f' v' e')
                    (IntMap.IntMap (VertexData f v e)) (IntMap.IntMap (VertexData f' v' e'))
_GraphIntMap = iso (\(Graph m) -> m) Graph

instance HasVertices' (GGraph f v e) where
  type Vertex   (GGraph f v e) = v
  type VertexIx (GGraph f v e) = Int

  -- | \(O(\log n)\)
  vertexAt i = vertexDataOf i <. vData
  numVertices (Graph m) = IntMap.size m

-- | Access the vertex data
vertexDataOf  :: VertexIx (GGraph f v e)
              -> IndexedTraversal' (VertexIx (GGraph f v e))
                                   (GGraph f v e)
                                   (VertexData f v e)
vertexDataOf i = _GraphIntMap .> iix i


instance HasVertices (GGraph f v e) (GGraph f v' e) where
  -- | running time: \(O(n)\).
  vertices = conjoined traverse' (itraverse' . indexed)
    where
      traverse'             :: Apply g => (v -> g v') -> GGraph f v e -> g (GGraph f v' e)
      traverse' f (Graph m) = Graph <$> traverseWithKey1' (\_ vd -> vd&vData %%~ f) m
      itraverse'             :: Apply g => (Int -> v -> g v') -> GGraph f v e -> g (GGraph f v' e)
      itraverse' f (Graph m) = Graph <$> traverseWithKey1' (\i vd -> vd&vData %%~ f i) m

-- | Unpacking the implementation of traverseWithKey for IntMap to work for Apply. Inp
-- principle is unsafe, but since we control how the GGraph's are constructed, the Nil
-- case should not occur.
traverseWithKey1'   :: Apply f => (Int -> v -> f v') -> IntMap.IntMap v -> f (IntMap.IntMap v')
traverseWithKey1' f = go
  where
    go = \case
      IntMapInternal.Tip k a       -> IntMapInternal.Tip k <$> f k a
      IntMapInternal.Bin prf m l r -> case (l,r) of
        (IntMapInternal.Nil,_) -> IntMapInternal.Bin prf m IntMapInternal.Nil             <$> go r
        (_,IntMapInternal.Nil) -> (\l' -> IntMapInternal.Bin prf m l' IntMapInternal.Nil) <$> go l
        _                      -> IntMapInternal.Bin prf m <$> go l Apply.<.> go r
      IntMapInternal.Nil           -> error "Hiraffe.AdjacencyListREp.vertices: no vertices!"
{-# INLINE traverseWithKey1' #-}


instance HasDarts' (GGraph f v e) where
  type Dart   (GGraph f v e) = e
  type DartIx (GGraph f v e) = (VertexIx (GGraph f v e), VertexIx (GGraph f v e))
                          -- lexicographical index

  -- | running time: \(O(\log m)\)
  dartAt (u,v) = vertexDataOf u <.> neighMap .> iix v

  -- | running time: O(n)
  numDarts (Graph m) = getSum . foldMap (Sum . lengthOf neighMap) $ m

instance HasDarts (GGraph f v e) (GGraph f v e') where
  -- | running time: \(O(m)\)
  darts pefe' (Graph m) = Graph <$> IntMap.traverseWithKey f m
    where
      f u vd = vd&neighMap.itraversed %%@~ \v e -> indexed pefe' (u,v) e

instance HasEdges' (GGraph f v e) where
  type Edge   (GGraph f v e) = Dart   (GGraph f v e)
  type EdgeIx (GGraph f v e) = DartIx (GGraph f v e)

  -- | running time: \(O(\log m)\)
  edgeAt e@(u',v') = let (u,v) = if u' <= v' then e else (v',u')
                     in vertexDataOf u <.> neighMap .> iix v

instance HasEdges (GGraph f v e) (GGraph f v e') where
  -- | running time: \(O(m)\)
  edges pefe' = fmap linkNegatives . darts (Indexed h)
    where
      h e@(u,v) x | u <= v    = Just <$> indexed pefe' e x
                  | otherwise = pure Nothing
      -- we compute the value only for the positive edges (i.e. u <=
      -- v), and wrap them in a Just. For the negative values we just
      -- store Nothings. We then use linkNegatives to, for each
      -- negative edge look up the value stored at the postiive edge.
  {-# INLINE edges #-}

-- | for each negative edge (v,u) look up the value stored at (u,v)
-- and store it here.
linkNegatives   :: GGraph f v (Maybe e') -> GGraph f v e'
linkNegatives g = g&darts %@~ \(u,v) x -> if u <= v then fromJust' x
                                                    else fromJust' $ g^?!dartAt (v,u)
  where
    fromJust' = fromMaybe (error "Hiraffe.AdjacencyListRep.edges: absurd ; fromJust")


instance HasFromFoldable f => DiGraph_ (GGraph f v e) where
  dirGraphFromAdjacencyLists =
    Graph . foldMap1 (\(i,v,adjs) -> let vd = VertexData v (mkNeighMap adjs) (mkNeighOrder adjs)
                                     in IntMap.singleton i vd
                     )
    where
      mkNeighMap   = foldMap (uncurry IntMap.singleton)
      mkNeighOrder = fromList . map fst . F.toList

  endPoints _ = id
  outNeighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (GGraph f v e) v
      asFold  = folding  $ \g -> g^..incidentEdges' u.asIndex.to (\v -> g^?! vertexAt v)
      asIFold = ifolding $ \g -> g^..incidentEdges' u.asIndex.to (\v -> (v, g^?! vertexAt v))
  {-# INLINE outNeighboursOf #-}

  twinDartOf (u,v) = to $ \g -> g^?dartAt (v,u).asIndex
    -- just look up the dart v,u


instance HasFromFoldable f => BidirGraph_ (GGraph f v e) where
  twinOf (u,v) = to $ const (v,u)
  getPositiveDart _ d@(u,v) | u <= v    = d
                            | otherwise = (v,u)
  -- we use the dart oriented from small to large as the positive one.

instance HasFromFoldable f => Graph_ (GGraph f v e) where
  fromAdjacencyLists =
    Graph . foldMap (\(i,v,adjs) -> let vd = VertexData v (mkNeighMap adjs) (mkNeighOrder adjs)
                                    in IntMap.singleton i vd
                    )
    where
      mkNeighMap   = foldMap (uncurry IntMap.singleton)
      mkNeighOrder = fromList . map fst . F.toList

  neighboursOf u = conjoined asFold asIFold
    where
      asFold  :: Fold (GGraph f v e) v
      asFold  = folding  $ \g -> g^..incidentEdges' u.asIndex.to (\v -> g^?! vertexAt v)
      asIFold = ifolding $ \g -> g^..incidentEdges' u.asIndex.to (\v -> (v, g^?! vertexAt v))
  {-# INLINE neighboursOf #-}

  incidentEdgesOf u = reindexed (u,) (incidentEdges' u)



-- | A traversal of the edges incident to vertex u as an intexed by the other vertex
incidentEdges'  :: VertexIx (GGraph f v e)
                -> IndexedTraversal' (VertexIx (GGraph f v e)) (GGraph f v e) e
-- incidentEdges'  :: (Indexable (VertexIx (GGraph f v e)) p, Applicative g)
--                 => VertexIx (GGraph f v e) -> p e (g e) -> GGraph f v e -> g (GGraph f v e)
incidentEdges' u = vertexDataOf u.neighMap.itraversed
