module Hiraffe.BFS.Pure
  ( bff
  , bfs
  ) where

import           Control.Lens hiding (Empty)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           Hiraffe.Graph.Class
import           Prelude hiding (filter)
import           Witherable

--------------------------------------------------------------------------------
-- * BFS

-- | Computes a breath first forest
-- (O(n \log n))
bff    :: (DiGraph_ graph, Ord (VertexIx graph))
       => graph -> [Tree (VertexIx graph)]
bff gr = go (foldMapOf (vertices.asIndex) Set.singleton gr)
  where
    go remaining = case Set.minView remaining of
      Nothing              -> []
      Just (v, remaining') -> let (remaining'', t) = bfs gr v remaining'
                              in t : go remaining''

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
