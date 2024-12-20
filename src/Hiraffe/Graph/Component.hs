{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.Graph.Component
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Connected Components
--
--------------------------------------------------------------------------------
module Hiraffe.Graph.Component
  ( HasConnectedComponents(..)
  , HasConnectedComponents'(..)
  ) where


import Control.Lens
import Data.Kind (Type)

--------------------------------------------------------------------------------


-- | A class representing types that have components.
class HasConnectedComponents' graph where
  {-# MINIMAL connectedComponentAt #-}

  -- | ConnectedComponents of the graph are of this type
  type ConnectedComponent   graph :: Type
  -- | ConnectedComponents are indexed by elements of type 'ConnectedComponentIx'
  type ConnectedComponentIx graph :: Type

  -- | Accessor to a given component.
  connectedComponentAt :: ConnectedComponentIx graph
                       -> IndexedTraversal' (ConnectedComponentIx graph)
                                            graph
                                            (ConnectedComponent graph)

  -- | Number of components in the graph.
  --
  -- running time: O(1)
  numConnectedComponents :: graph -> Int
  default numConnectedComponents :: HasConnectedComponents graph graph => graph -> Int
  numConnectedComponents = lengthOf connectedComponents


-- | Class that expresses that we have a non-empty type changing traversal of all components.
--
class HasConnectedComponents' graph => HasConnectedComponents graph graph' where
  -- | Traversal of all components in the graph
  connectedComponents :: IndexedTraversal1 (ConnectedComponentIx graph)
                                           graph graph'
                                           (ConnectedComponent graph)
                                           (ConnectedComponent graph')
