--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.World
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines a data type to distinguish primal and dual graphs.
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.World
  ( World(..)
  , DualOf, dualDualIdentity
  ) where

import Data.Type.Equality
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- * Representing The World

-- | The world in which the graph lives
data World = Primal | Dual deriving (Show,Eq)

-- | We can take the dual of a world. For the Primal this gives us the Dual,
-- for the Dual this gives us the Primal.
type family DualOf (sp :: World) where
  DualOf Primal = Dual
  DualOf Dual   = Primal

-- | The Dual of the Dual is the Primal.
dualDualIdentity :: forall w. DualOf (DualOf w) :~: w
dualDualIdentity = unsafeCoerce Refl
          -- manual proof:
          --    DualOf (DualOf Primal) = Primal
          --    DualOf (DualOf Dual)   = Dual
