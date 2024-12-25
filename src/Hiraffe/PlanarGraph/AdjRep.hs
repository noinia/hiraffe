{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.AdjRep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types that to represent a planar graph as Adjacency Lists. The main
-- purpose is to help encode/decode a PlanarGraph as a JSON/YAML file.
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.AdjRep where

import           Control.Lens (Bifunctor (..))
import           Data.Aeson (ToJSON(..),FromJSON(..),genericToEncoding,defaultOptions)
import           Data.Bifoldable
import           Data.Bifunctor (second)
import           Data.Bitraversable
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.YAML
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Data type representing the graph in its JSON/Yaml format
data Gr v f = Gr { adjacencies :: NonEmpty v
                 , faces       :: NonEmpty f
                 } deriving (Generic, Show, Eq)

instance Functor (Gr v) where
  fmap f (Gr vs fs) = Gr vs (fmap f fs)
instance Foldable (Gr v) where
  foldMap f (Gr _ fs) = foldMap f fs
instance Traversable (Gr v) where
  traverse f (Gr vs fs) = Gr vs <$> traverse f fs

instance Bifunctor Gr where
  bimap f g (Gr vs fs) = Gr (fmap f vs) (fmap g fs)
instance Bifoldable Gr where
  bifoldMap f g (Gr vs fs) = foldMap f vs <> foldMap g fs
instance Bitraversable Gr where
  bitraverse f g (Gr vs fs) = Gr <$> traverse f vs <*> traverse g fs


instance (ToYAML v, ToYAML f)     => ToYAML   (Gr v f) where
  toYAML (Gr vs fs) = mapping [ "adjacencies" .= F.toList vs
                              , "faces"       .= F.toList fs
                              ]

instance (FromYAML v, FromYAML f) => FromYAML   (Gr v f) where
  parseYAML = withMap "Gr" $ \m -> Gr <$> (m .: "adjacencies" >>= toNonEmpty')
                                      <*> (m .: "faces"       >>= toNonEmpty')
    where
      toNonEmpty' l = case NonEmpty.nonEmpty l of
                        Just xs -> pure xs
                        Nothing -> fail "found an empty list, expected non-empty list"

instance (ToJSON v, ToJSON f)     => ToJSON   (Gr v f) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON v, FromJSON f) => FromJSON (Gr v f)

----------------------------------------

-- | A vertex, represented by an id, its adjacencies, and its data.
data Vtx v e = Vtx { id    :: {-# UNPACK #-} !Int
                   , adj   :: [(Int,e)]
                     -- ^ adjacent vertices + data on the edge. Some functions, like
                     -- 'fromAdjRep' may assume that the adjacencies are given in
                     -- counterclockwise order around the vertices. This is not (yet)
                     -- enforced by the data type.
                     --
                     -- Note that this data type allows the edges (u,v) and (v,u) to store
                     -- different data.
                   , vData :: !v
                   } deriving (Generic, Show, Eq,Functor,Foldable,Traversable)

-- instance Functor (Vtx v) where
--   fmap f (Vtx i as x) = Vtx i (map (second f) as) x
-- instance Foldable (Vtx v) where
--   foldMap f (Vtx _ ads _) = foldMap (f . snd) ads
-- instance Traversable (Vtx v) where
--   traverse f (Vtx i adjs x) = Vtx i <$> traverse (traverse f) adjs <*> pure x

instance Bifunctor Vtx where
  bimap f g (Vtx i as x) = Vtx i (map (second g) as) (f x)
instance Bifoldable Vtx where
  bifoldMap f g (Vtx _ ads x) = foldMap (g . snd) ads <> f x
instance Bitraversable Vtx where
  bitraverse f g (Vtx i adjs x) = Vtx i <$> traverse (traverse g) adjs <*> f x


instance (ToYAML v, ToYAML e)     => ToYAML   (Vtx v e) where
  toYAML (Vtx i as x) = mapping [ "id"   .= i
                                , "adj"  .= as
                                , "data" .= x
                                ]

instance (FromYAML v, FromYAML e) => FromYAML   (Vtx v e) where
  parseYAML = withMap "Vtx" $ \m -> Vtx <$> m .: "id" <*> m .: "adj" <*> m .: "data"

instance (ToJSON v, ToJSON e)     => ToJSON   (Vtx v e) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON v, FromJSON e) => FromJSON (Vtx v e)

----------------------------------------

-- | Faces
data Face f = Face { incidentEdge :: (Int,Int) -- ^ an edge (u,v) s.t. the face
                                               -- is right from (u,v)
                   , fData        :: !f
                   } deriving (Generic,Functor,Foldable,Traversable,Show, Eq)

instance ToYAML f => ToYAML   (Face f) where
  toYAML (Face e x) = mapping [ "incidentEdge" .= e
                              , "data"         .= x
                              ]

instance FromYAML f => FromYAML (Face f) where
  parseYAML = withMap "Face" $ \m -> Face <$> m .: "incidentEdge" <*> m .: "data"

instance ToJSON f   => ToJSON   (Face f) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON f => FromJSON (Face f)


--------------------------------------------------------------------------------

-- instance HasVertices' (Gr (Vtx v e) f) where
--   type Vertex   (Gr (Vtx v e) f) = v
--   type VertexIx (Gr (Vtx v e) f) = Int
--   vertexAt u =

-- instance HasVertices (Gr (Vtx v e) f) (Gr (Vtx v' e) f) where
--   vertices = adjacencies
