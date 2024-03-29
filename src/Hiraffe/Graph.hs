--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.Graph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines various relevant graph classes
--
--------------------------------------------------------------------------------
module Hiraffe.Graph
  ( HasVertices(..), HasVertices'(..)
  , HasDarts(..), HasDarts'(..)
  , HasEdges(..), HasEdges'(..)

  , Graph_(..)
  , DiGraph_(..)
  , BidirGraph_(..)
  ) where

import Hiraffe.Graph.Class
