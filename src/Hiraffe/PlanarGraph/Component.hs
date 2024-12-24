{-# LANGUAGE TypeData #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Component
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Connected Components
--
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Component
  ( HasConnectedComponents(..)
  , HasConnectedComponents'(..)

  , FaceData(FaceData)
  , holes, fData

  , Wrap --, Wrap'
  , ComponentId(..)
  , Raw(Raw), dataVal
  , RawFace(RawFace), faceIdx, faceDataVal
  ) where


import           Control.Lens hiding (holes)
import           Data.Aeson
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Hiraffe.Graph.Component
import           Hiraffe.PlanarGraph.Connected (FaceId)
import           Hiraffe.PlanarGraph.Dart (Dart)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * ComponentId

-- | ComponentId type
newtype ComponentId (s :: k) = ComponentId { unCI :: Int }
  deriving (Show,Eq,Ord,Generic,Bounded,Enum,ToJSON,FromJSON)


--------------------------------------------------------------------------------
-- * FaceData

-- | The Face data consists of the data itself and a list of holes
data FaceData h f = FaceData { _holes :: Seq.Seq h
                             , _fData :: !f
                             } deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic)

-- | lens to access the holes of a face
holes :: Lens (FaceData h f) (FaceData h' f) (Seq.Seq h) (Seq.Seq h')
holes = lens _holes (\fd hs -> fd { _holes = hs })
{-# INLINE holes #-}

-- | Lens to access the actual face data
fData :: Lens (FaceData h f) (FaceData h f') f f'
fData = lens _fData (\fd x -> fd { _fData = x })
{-# INLINE fData #-}

instance Bifunctor FaceData where
  bimap f g (FaceData hs x) = FaceData (fmap f hs) (g x)


instance (FromJSON h, FromJSON f) => FromJSON (FaceData h f)
instance (ToJSON h, ToJSON f)     => ToJSON (FaceData h f) where
  toEncoding = genericToEncoding defaultOptions


--------------------------------------------------------------------------------
-- * Helper for storing information about multiple components

-- | Helper data type and type family to Wrap a proxy type.
type data Wrap (s :: k)
-- type family Wrap (s :: k) :: Type where
--   Wrap s = Wrap' s

--------------------------------------------------------------------------------

-- | Helper type for the data that we store in a planar graph
data Raw s ia a = Raw { _compId  :: {-# UNPACK #-} !(ComponentId s)
                      , _idxVal  :: !ia
                      , _dataVal :: !a
                      } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

instance (FromJSON ia, FromJSON a) => FromJSON (Raw s ia a)
instance (ToJSON ia, ToJSON a) => ToJSON (Raw s ia a) where
  toEncoding = genericToEncoding defaultOptions

instance FunctorWithIndex i (Raw ci i) where
  imap f (Raw ci i x) = Raw ci i (f i x)
instance FoldableWithIndex i (Raw ci i) where
  ifoldMap f (Raw _ i x) = f i x
instance TraversableWithIndex i (Raw ci i) where
  itraverse f (Raw ci i x) = Raw ci i <$> f i x

-- | get the dataVal of a Raw
dataVal :: Lens (Raw s ia a) (Raw s ia b) a b
dataVal = lens (\(Raw _ _ x) -> x) (\(Raw c i _) y -> Raw c i y)

--------------------------------------------------------------------------------

-- | Face data, if the face is an inner face, store the component and
-- faceId of it.  If not, this face must be the outer face (and thus
-- we can find all the face id's it correponds to through the
-- FaceData).
data RawFace s f = RawFace { _faceIdx     :: !(Maybe (ComponentId s, FaceId (Wrap s)))
                           , _faceDataVal :: !(FaceData (Dart s) f)
                           } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

-- TODO: use unpacked/strict values for the faceIdx

-- | Lens to access the faceIx (if it exists)
faceIdx :: Lens' (RawFace s f) (Maybe (ComponentId s, FaceId (Wrap s)))
faceIdx = lens _faceIdx (\rf x -> rf { _faceIdx = x })
{-# INLINE faceIdx #-}

-- | Lens to access the face data
faceDataVal :: Lens (RawFace s f)  (RawFace s f') (FaceData (Dart s) f) (FaceData (Dart s) f')
faceDataVal = lens _faceDataVal (\rf x -> rf { _faceDataVal = x })
{-# INLINE faceDataVal #-}
