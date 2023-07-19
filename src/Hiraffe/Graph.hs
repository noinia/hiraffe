{-# LANGUAGE DefaultSignatures #-}
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
  , HasEdges(..), HasEdges'(..)
  , HasFaces(..), HasFaces'(..)

  , Graph_(..)
  , DirGraph_
  , PlanarGraph_
  ) where

import Hiraffe.Graph.Class
