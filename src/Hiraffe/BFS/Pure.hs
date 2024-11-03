module Hiraffe.BFS.Pure
  ( bff
  , bfs
  ) where

import           Control.Lens hiding (Empty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import           Data.Tree
import           Hiraffe.Graph.Class
import           Prelude hiding (filter)
import           Witherable

--------------------------------------------------------------------------------
-- * BFS

-- | Computes a breath first forest
--
-- (O(n \log n))
bff    :: (DiGraph_ graph, Ord (VertexIx graph))
       => graph -> NonEmpty.NonEmpty (Tree (VertexIx graph))
bff gr = go (foldMapOf (vertices.asIndex) NESet.singleton gr)
  where
    go remaining = let (v, remaining') = NESet.deleteFindMin remaining
                   in case bfs gr v remaining' of
                        (NESet.IsEmpty, t)                -> NonEmpty.singleton t
                        (NESet.IsNonEmpty remaining'', t) -> t NonEmpty.<| go remaining''

-- | Turn the map into a tree.
toTree   :: Ord k => Map k [k] -> k -> Tree k
toTree m = go
  where
    go s = Node s $ map go (fromMaybe [] $ Map.lookup s m)

-- | BFS from the given starting vertex, and the set of still-to-visit vertices.
-- returns the remaining still-to-visit vertices and the tree.
bfs      :: (DiGraph_ graph, Ord (VertexIx graph))
         => graph -> VertexIx graph
         -> Set (VertexIx graph)
         -> (Set (VertexIx graph), Tree (VertexIx graph))
bfs gr s = fmap (flip toTree s) . bfs' [s]
  where
    bfs' lvl remaining = foldr visit (remaining, Map.empty) lvl

    visit v (remaining, m) = let chs = filter (flip Set.member remaining) $ neighs v
                             in (foldr Set.delete remaining chs, Map.insert v chs m)

    neighs v = gr^..outNeighboursOf v.asIndex



--------------------------------------------------------------------------------
