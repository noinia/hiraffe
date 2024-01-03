--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.BFS
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Hiraffe.BFS
  ( bfs
  ) where

import           Control.Lens hiding (Empty)
import           Control.Monad.ST.Strict
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Tree
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Hiraffe.Graph.Class
import           Witherable

--------------------------------------------------------------------------------

-- bfs      :: Foldable f => Int -> V.Vector (v, f Int) -> Tree v
-- bfs s gr = fmap (fst . (gr V.!)) . bfs' s . fmap snd $ gr

-- -- | Runs a BFS from the first vertex in the graph. The graph is given
-- -- in adjacency list representation.
-- --
-- -- running time: \(O(V + E)\)
-- bfs'      :: Foldable f => Int -> V.Vector (f Int) -> Tree Int
--         => graph -> VertexIx graph -> Tree (VertexIx graph)
--


-- | Runs a BFS from the first vertex in the graph. The graph is given
-- in adjacency list representation.
--
-- pre: the vertices in the graph are numbered (0,n-1)
--
-- running time: \(O(V + E)\)
bfs      :: forall graph. (DirGraph_ graph, Enum (VertexIx graph))
         => graph -> VertexIx graph -> Tree (VertexIx graph)
bfs gr s = extract s $ V.create
         $ do st  <- UMV.replicate n False
              out <- MV.new n
              go0 st out (s :<| mempty)
              pure out
  where
    n = numVertices gr
    go0        :: forall s. UMV.MVector s Bool -> MV.MVector s [VertexIx graph]
               -> Seq (VertexIx graph) -> ST s ()
    go0 st out = go
      where
        visit i = do b <- UMV.read st (fromEnum i)
                     UMV.write st (fromEnum i) True -- mark i as visited
                     pure $ if b then Nothing else Just i

        go :: Seq (VertexIx graph) -> ST s ()
        go = \case
          Empty       -> pure ()
          (u:<|queue) -> do ns <- wither visit $ gr^..outNeighboursOf u.asIndex
                            MV.write out (fromEnum u) ns -- write that u's children are ns
                            go (queue <> Seq.fromList ns)


-- | Give na root index and a vector s.t. v[i] lists the children of
-- node i, builds the acutal tree.
extract     :: Enum i => i -> V.Vector [i] -> Tree i
extract s v = go s
  where
    go i = Node i (map go $ v V.! fromEnum i)


--------------------------------------------------------------------------------
