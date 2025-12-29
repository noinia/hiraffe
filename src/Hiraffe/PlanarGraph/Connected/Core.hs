--------------------------------------------------------------------------------
-- |
-- Module      :  Hiraffe.PlanarGraph.Connected.Core
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing connected planar graphs
--------------------------------------------------------------------------------
module Hiraffe.PlanarGraph.Connected.Core
  ( VertexIdIn(..), VertexId, unVertexId
  , FaceIdIn(..), FaceId
  , DartId

  , CPlanarGraph
  , embedding, vertexData, dartData, faceData, dual
  , dartVector

  , traverseVertices, traverseDarts, traverseFaces
  , cPlanarGraph', cPlanarGraph
  , toAdjacencyLists

  , numVertices, numDarts, numEdges, numFaces
  , vertices', vertices
  , darts', darts
  , edges', edges
  , tailOf, headOf
  , endPoints
  , incidentEdges, incomingEdges, outgoingEdges
  , neighboursOf
  , nextIncidentEdge, prevIncidentEdge
  , nextIncidentEdgeFrom, prevIncidentEdgeFrom
  , HasDataOf(..)
  , endPointDataOf
  , computeDual

  , unsafeChangeS
  ) where

import           Control.DeepSeq
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Default.Class
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Data.Traversable
import           Data.Type.Equality (gcastWith)
import qualified Data.Vector.Mutable as MV
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as V
import           GHC.Generics (Generic)
import           HGeometry.Permutation
import           HGeometry.Vector.NonEmpty.Util ()
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Hiraffe.PlanarGraph.World
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder

--------------------------------------------------------------------------------
-- $setup
-- >>> import Hiraffe.PlanarGraph.Dart(Dart(Dart),Arc(Arc),Direction(..))
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]
--     adjacencies = NonEmpty.fromList . fmap (fmap NonEmpty.fromList) $
--                           [ ("u"
--                             , [ (Dart.Dart aA Negative, "a-")
--                               , (Dart.Dart aC Positive, "c+")
--                               , (Dart.Dart aB Positive, "b+")
--                               , (Dart.Dart aA Positive, "a+")
--                               ]
--                             )
--                           , ("v"
--                             , [ (Dart.Dart aE Negative, "e-")
--                               , (Dart.Dart aB Negative, "b-")
--                               , (Dart.Dart aD Negative, "d-")
--                               , (Dart.Dart aG Positive, "g+")
--                               ]
--                             )
--                           , ("w"
--                             , [ (Dart.Dart aE Positive, "e+")
--                               , (Dart.Dart aD Positive, "d+")
--                               , (Dart.Dart aC Negative, "c-")
--                               ]
--                             )
--                           , ("x"
--                             , [ (Dart.Dart aG Negative, "g-")
--                               ]
--                             )
--                           ]
--     myGraph :: CPlanarGraph Primal String String String String
--     myGraph = cPlanarGraph adjacencies
--                    & faceData   .~ V.unsafeFromList ["f_3", "f_infty","f_1","f_2"]
--     showWithData     :: HasDataOf s i => s -> i -> (i, DataOf s i)
--     showWithData g i = (i, g^.dataOf i)
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Hiraffe/PlanarGraph/Connected/testG.png)

--------------------------------------------------------------------------------
-- * VertexId's

-- | A vertex in a planar graph. A vertex is tied to a particular planar graph
-- by the phantom type s, and to a particular world w.
newtype VertexIdIn (w :: World) (s :: k) = VertexId { _unVertexId :: Int }
                                         deriving (Eq,Ord,Enum,ToJSON,FromJSON,Generic,NFData)
-- VertexId's are in the range 0...|orbits|-1

-- | Shorthand for vertices in the primal.
type VertexId = VertexIdIn Primal

-- | Getter for a VertexId's unique number.
unVertexId :: Getter (VertexIdIn w s) Int
unVertexId = to _unVertexId

instance Show (VertexIdIn w s) where
  showsPrec d (VertexId i) = showParen (d >= 11)
                                       (showString "VertexId " . showsPrec 11 i)

-- not entirely ideal; but it simplifies our job in hgeometry.
instance Default (VertexIdIn w s) where
  def = VertexId 0

--------------------------------------------------------------------------------
-- * FaceId's

-- | The type to represent FaceId's
newtype FaceIdIn w (s :: k) = FaceId { _unFaceId :: VertexIdIn (DualOf w) s }
                            deriving (Eq,Ord,Enum,ToJSON,FromJSON,Generic)
                            deriving newtype (NFData)

-- | Shorthand for FaceId's in the primal.
type FaceId = FaceIdIn Primal

instance Show (FaceIdIn w s) where
  showsPrec d (FaceId (VertexId i)) = showParen (d >= 11)
                                                (showString "FaceId " . showsPrec 11 i)

instance Default (FaceIdIn w s) where
  def = FaceId (VertexId 0)
--------------------------------------------------------------------------------

-- | Type alias to prevent confusion with the Dart type from the HasDart' typeclass.
type DartId s = Dart.Dart s

--------------------------------------------------------------------------------
-- * The graph type itself

-- | A *connected* Planar graph with bidirected edges. I.e. the edges (darts) are
-- directed, however, for every directed edge, the edge in the oposite
-- direction is also in the graph.
--
-- The types v, e, and f are the are the types of the data associated with the
-- vertices, edges, and faces, respectively.
--
-- The orbits in the embedding are assumed to be in counterclockwise
-- order. Therefore, every dart directly bounds the face to its right.
data CPlanarGraph (w :: World) (s :: k) v e f =
  CPlanarGraph { _embedding   :: Permutation (DartId s)
               , _vertexData  :: NonEmptyVector v
               , _dartData    :: NonEmptyVector e
               -- ^ For every dart (so both negative and positive) we can store some data.
               , _faceData    :: NonEmptyVector f
               , _dual        :: CPlanarGraph (DualOf w) s f e v
               } deriving (Generic)

-- | Change the s type involved
--
-- O(n).
--
--
unsafeChangeS :: forall w s s' v e f. CPlanarGraph w s v e f -> CPlanarGraph w s' v e f
unsafeChangeS (CPlanarGraph emb vtx ds fs (CPlanarGraph dualE dualV dualD dualF _)) = g
  where
    g  = CPlanarGraph (fmap coerce' emb)   vtx   ds    fs    dg
    dg = CPlanarGraph (fmap coerce' dualE) dualV dualD dualF (gcastWith proof g)
    proof = dualDualIdentity :: DualOf (DualOf w) :~: w
    coerce'                            :: Dart.Dart s -> Dart.Dart s'
    coerce' (Dart.Dart (Dart.Arc a) d) = Dart.Dart (Dart.Arc a) d
  -- TODO: this should really just be coerce or unsafeCoerce, but somehow GHC
  -- then loops because it does not want to coerce to s.t. of a different kind
{-# INLINE unsafeChangeS #-}

instance Functor (CPlanarGraph w s v e) where
  fmap = fmapDefault
instance Foldable (CPlanarGraph w s v e) where
  foldMap f = foldMap f . _faceData
instance Traversable (CPlanarGraph w s v e) where
  traverse f (CPlanarGraph e vs ds fs (CPlanarGraph e' _ ds' fs' _)) =
    (\fsNew -> let g     = CPlanarGraph e  vs    ds  fsNew dualG
                   dualG = CPlanarGraph e' fsNew ds' fs'   (gcastWith proof g)
                   proof = dualDualIdentity :: DualOf (DualOf w) :~: w
               in g
    ) <$> traverse f fs
instance Bifunctor (CPlanarGraph w s v) where
  bimap f g = runIdentity . bitraverse (Identity . f) (Identity . g)
instance Bifoldable (CPlanarGraph w s v) where
  bifoldMap f g (CPlanarGraph _ _ ds fs _) = foldMap f ds <> foldMap g fs
instance Bitraversable (CPlanarGraph w s v) where
  bitraverse f g (CPlanarGraph e vs ds fs (CPlanarGraph e' _ _ fs' _)) =
    (\dsNew fsNew -> let gr    = CPlanarGraph e  vs    dsNew fsNew dualG
                         dualG = CPlanarGraph e' fsNew dsNew fs'   (gcastWith proof gr)
                         proof = dualDualIdentity :: DualOf (DualOf w) :~: w
                     in gr
    ) <$> traverse f ds
      <*> traverse g fs

instance (Show v, Show e, Show f) => Show (CPlanarGraph w s v e f) where
  show (CPlanarGraph e v r f _) = unwords [ "CPlanarGraph"
                                          , "embedding =", show e
                                          , ", vertexData =", show v
                                          , ", dartData =", show r
                                          , ", faceData =", show f
                                          ]
  -- TODO: this is not a very good show instance; implement showsPrec instead

instance (Eq v, Eq e, Eq f) => Eq (CPlanarGraph w s v e f) where
  (CPlanarGraph e v r f _) == (CPlanarGraph e' v' r' f' _) =  e == e' && v == v'
                                                         && r == r' && f == f'
  {-# INLINE (==) #-}

instance (NFData v, NFData e, NFData f) => NFData (CPlanarGraph w s v e f)

-- ** lenses and getters

-- | Get the embedding, represented as a permutation of the darts, of this
-- graph.
embedding :: Getter (CPlanarGraph w s v e f) (Permutation (DartId s))
embedding = to _embedding

-- | \(O(1)\) access, \( O(n) \) update.
vertexData :: Lens (CPlanarGraph w s v e f) (CPlanarGraph w s v' e f)
                   (NonEmptyVector v) (NonEmptyVector v')
vertexData = lens _vertexData (\g vD -> updateData (const vD) id id g)

-- | \(O(1)\) access, \( O(n) \) update.
dartData :: Lens (CPlanarGraph w s v e f) (CPlanarGraph w s v e' f)
                 (NonEmptyVector e)       (NonEmptyVector e')
dartData = lens _dartData (\g dD -> updateData id (const dD) id g)

-- | \(O(1)\) access, \( O(n) \) update.
faceData :: Lens (CPlanarGraph w s v e f) (CPlanarGraph w s v e f')
                 (NonEmptyVector f) (NonEmptyVector f')
faceData = lens _faceData (\g fD -> updateData id id (const fD) g)

-- | Get the dual graph of this graph.
dual :: Getter (CPlanarGraph w s v e f) (CPlanarGraph (DualOf w) s f e v)
dual = to _dual

-- | lens to access the vector with DartId s with their data. This function makes sure that
-- if you somehow reorder the edge data it is assigned correctly again.
--
-- \(O(1)\) access, \( O(n) \) update.
dartVector :: Lens (CPlanarGraph w s v e f) (CPlanarGraph w s v e' f)
                   (NonEmptyVector (DartId s, e))  (NonEmptyVector (DartId s, e'))
dartVector = lens darts (\g dD -> updateData id (const $ reorderEdgeData dD) id g)
{-# INLINE dartVector #-}

-- -- | edgeData is just an alias for 'dartVector'
-- edgeData :: Lens (CPlanarGraph w s v e f) (CPlanarGraph w s v e' f)
--                  (V.Vector (DartId s, e)) (V.Vector (DartId s, e'))
-- edgeData = dartVector

-- | Helper function to update the data in a planar graph. Takes care to update
-- both the data in the original graph as well as in the dual.
updateData :: forall s w v e f v' e' f'
           .  (NonEmptyVector v -> NonEmptyVector v')
           -> (NonEmptyVector e -> NonEmptyVector e')
           -> (NonEmptyVector f -> NonEmptyVector f')
           -> CPlanarGraph w s v  e  f
           -> CPlanarGraph w s v' e' f'
updateData = gcastWith proof updateData'
  where
    proof :: DualOf (DualOf w) :~: w
    proof = dualDualIdentity

-- | The function that does the actual work for 'updateData'
updateData'  :: (DualOf (DualOf w) ~ w)
             => (NonEmptyVector v -> NonEmptyVector v')
             -> (NonEmptyVector e -> NonEmptyVector e')
             -> (NonEmptyVector f -> NonEmptyVector f')
             -> CPlanarGraph w s v  e  f
             -> CPlanarGraph w s v' e' f'
updateData' fv fe ff (CPlanarGraph em vtxData dData fData dg) = g'
  where
    vtxData' = fv vtxData
    dData'   = fe dData
    fData'   = ff fData

    g'       = CPlanarGraph em              vtxData' dData' fData'   dg'
    dg'      = CPlanarGraph (dg^.embedding) fData'   dData' vtxData' g'






-- | Reorders the edge data to be in the right order to set dartData
reorderEdgeData    :: Foldable1 f => f (DartId s, e) -> NonEmptyVector e
reorderEdgeData ds = V.unsafeCreate $ do
                                  v <- MV.new (F.length ds)
                                  F.forM_ ds $ \(d,x) ->
                                    MV.write v (fromEnum d) x
                                  pure v
  -- note, this is actually safe since the input is non-empty


-- | Traverse the vertices
--
-- >>> (^.vertexData) <$> traverseVertices (\i x -> Just (i,x)) myGraph
-- Just [(VertexId 0,"u"),(VertexId 1,"v"),(VertexId 2,"w"),(VertexId 3,"x")]
-- >>> traverseVertices (\i x -> print (i,x)) myGraph >> pure ()
-- (VertexId 0,"u")
-- (VertexId 1,"v")
-- (VertexId 2,"w")
-- (VertexId 3,"x")
traverseVertices   :: Apply m
                   => (VertexIdIn w s -> v -> m v')
                   -> CPlanarGraph w s v e f
                   -> m (CPlanarGraph w s v' e f)
traverseVertices f = itraverseOf (vertexData.traversed1) (f . VertexId)

-- | Traverses the darts
--
-- >>> traverseDarts (\d x -> print (d,x)) myGraph >> pure ()
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 0) -1,"a-")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 2) -1,"c-")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 4) -1,"e-")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 5) -1,"g-")
traverseDarts   :: Apply m
                => (DartId s -> e -> m e')
                -> CPlanarGraph w s v e f
                -> m (CPlanarGraph w s v e' f)
traverseDarts f = itraverseOf (dartData.traversed1) (f . toEnum)

-- | Traverses the faces
--
-- >>> traverseFaces (\i x -> print (i,x)) myGraph >> pure ()
-- (FaceId 0,"f_3")
-- (FaceId 1,"f_infty")
-- (FaceId 2,"f_1")
-- (FaceId 3,"f_2")
traverseFaces   :: Apply m
                => (FaceIdIn w s -> f -> m f')
                -> CPlanarGraph w s v e f
                -> m (CPlanarGraph w s v e f')
traverseFaces f = itraverseOf (faceData.traversed1) (\i -> f (FaceId $ VertexId i))

--------------------------------------------------------------------------------
-- ** Constructing a Planar graph

-- | Construct a planar graph
--
-- running time: \(O(n)\).
cPlanarGraph'      :: Permutation (DartId s) -> CPlanarGraph w s () () ()
cPlanarGraph' perm = pg
  where
    pg = CPlanarGraph perm vData eData fData (computeDual pg)
        -- note the lazy calculation of computeDual that refers to pg itself
    d  = size perm
    e  = d `div` 2
    v  = V.length (perm^.orbits)
    f  = e - v + 2

    vData  = V.replicate1 v ()
    eData  = V.replicate1 d ()
    fData  = V.replicate1 f ()

-- | Construct a planar graph, given the darts in cyclic order around each
-- vertex.
--
-- running time: \(O(n)\).
cPlanarGraph    :: Foldable1 nonEmpty
                => nonEmpty (v, NonEmpty (DartId s,e)) -> CPlanarGraph Primal s v e ()
cPlanarGraph xs = cPlanarGraph' perm & dartVector .~ V.fromNonEmpty (sconcat ds)
                                     & vertexData .~ V.unsafeFromVector (Builder.build vs)
  where
    (vs,ds) = foldMap1 (\(v, adjs) -> (Builder.singleton v, NonEmpty.singleton adjs)) xs
    n       = sum . fmap length $ ds
    perm    = toCycleRep n $ fmap (fmap fst) ds


-- | Produces the adjacencylists for all vertices in the graph. For every vertex, the
-- adjacent vertices are given in counter clockwise order.
--
-- Note that in case a vertex u as a self loop, we have that this vertexId occurs
-- twice in the list of neighbours, i.e.: u : [...,u,..,u,...]. Similarly, if there are
-- multiple darts between a pair of edges they occur multiple times.
--
-- running time: \(O(n)\)
toAdjacencyLists    :: CPlanarGraph w s v e f
                    -> NonEmpty (VertexIdIn w s, NonEmptyVector (VertexIdIn w s))
toAdjacencyLists pg = fmap (\u -> (u,neighboursOf u pg)) . toNonEmpty . vertices' $ pg
-- TODO: something weird happens when we have self-loops here.


--------------------------------------------------------------------------------
-- ** Convenience functions

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 4
numVertices :: CPlanarGraph w s v e f -> Int
numVertices g = V.length (g^.embedding.orbits)

-- | Get the number of Darts
--
-- >>> numDarts myGraph
-- 12
numDarts :: CPlanarGraph w s v e f -> Int
numDarts g = size (g^.embedding)

-- | Get the number of Edges
--
-- >>> numEdges myGraph
-- 6
numEdges :: CPlanarGraph w s v e f -> Int
numEdges g = numDarts g `div` 2

-- | Get the number of faces
--
-- >>> numFaces myGraph
-- 4
numFaces   :: CPlanarGraph w s v e f -> Int
numFaces g = numEdges g - numVertices g + 2


-- | Enumerate all vertices
--
-- >>> vertices' myGraph
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: CPlanarGraph w s v e f -> NonEmptyVector (VertexIdIn w s)
vertices' g = VertexId <$> V.enumFromN1 0 (V.length (g^.embedding.orbits))

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices   :: CPlanarGraph w s v e f -> NonEmptyVector (VertexIdIn w s, v)
vertices g = V.zip (vertices' g) (g^.vertexData)



-- | Enumerate all darts
darts' :: CPlanarGraph w s v e f -> NonEmptyVector (DartId s)
darts' = elems . _embedding

-- | Get all darts together with their data
--
-- >>> mapM_ print $ darts myGraph
-- (Dart (Arc 0) -1,"a-")
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 4) -1,"e-")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 2) -1,"c-")
-- (Dart (Arc 5) -1,"g-")
darts   :: CPlanarGraph w s v e f -> NonEmptyVector (DartId s, e)
darts g = (\d -> (d,g^.dataOf d)) <$> darts' g

-- | Enumerate all edges. We report only the Positive darts
edges' :: CPlanarGraph w s v e f -> NonEmptyVector (DartId s)
edges' = V.unsafeFromVector . V.filter Dart.isPositive . darts'

-- | Enumerate all edges with their edge data. We report only the Positive
-- darts.
--
-- >>> mapM_ print $ edges myGraph
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
edges :: CPlanarGraph w s v e f -> NonEmptyVector (DartId s, e)
edges = V.unsafeFromVector . V.filter (Dart.isPositive . fst) . darts


-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
-- >>> showWithData myGraph $ tailOf (Dart (Arc 3) Positive) myGraph
-- (VertexId 2,"w")
--
-- running time: \(O(1)\)
tailOf     :: DartId s -> CPlanarGraph w s v e f -> VertexIdIn w s
tailOf d g = VertexId . fst $ lookupIdx (g^.embedding) d

-- | The vertex this dart is heading in to
--
-- showWithData myGraph $ headOf (Dart (Arc 3) Positive) myGraph
-- (VertexId 1,"v")
--
-- running time: \(O(1)\)
headOf   :: DartId s -> CPlanarGraph w s v e f -> VertexIdIn w s
headOf d = tailOf (Dart.twin d)

-- | endPoints d g = (tailOf d g, headOf d g)
--
-- >>> endPoints (Dart (Arc 3) Positive) myGraph
-- (VertexId 2,VertexId 1)
--
-- running time: \(O(1)\)
endPoints :: DartId s -> CPlanarGraph w s v e f -> (VertexIdIn w s, VertexIdIn w s)
endPoints d g = (tailOf d g, headOf d g)

-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- >>> incidentEdges (VertexId 1) myGraph
-- [Dart (Arc 4) -1,Dart (Arc 1) -1,Dart (Arc 3) -1,Dart (Arc 5) +1]
-- >>> mapM_ (print . showWithData myGraph) $ incidentEdges (VertexId 1) myGraph
-- (Dart (Arc 4) -1,"e-")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 5) +1,"g+")
-- >>> mapM_ (print . showWithData myGraph) $ incidentEdges (VertexId 3) myGraph
-- (Dart (Arc 5) -1,"g-")
--
-- running time: \(O(k)\), where \(k\) is the output size
incidentEdges                :: VertexIdIn w s -> CPlanarGraph w s v e f
                             -> NonEmptyVector (DartId s)
incidentEdges (VertexId v) g = g^?!embedding.orbits.ix v

-- | All edges incident to vertex v in incoming direction
-- (i.e. pointing into v) in counterclockwise order around v.
--
-- >>> incomingEdges (VertexId 1) myGraph
-- [Dart (Arc 4) +1,Dart (Arc 1) +1,Dart (Arc 3) +1,Dart (Arc 5) -1]
-- >>> mapM_ (print . showWithData myGraph) $ incomingEdges (VertexId 1) myGraph
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 5) -1,"g-")
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
incomingEdges     :: VertexIdIn w s -> CPlanarGraph w s v e f -> NonEmptyVector (DartId s)
incomingEdges v g = orient <$> incidentEdges v g
  where
    orient d = if headOf d g == v then d else Dart.twin d

-- | All edges incident to vertex v in outgoing direction
-- (i.e. pointing away from v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
outgoingEdges     :: VertexIdIn w s -> CPlanarGraph w s v e f -> NonEmptyVector (DartId s)
outgoingEdges v g = orient <$> incidentEdges v g
  where
    orient d = if tailOf d g == v then d else Dart.twin d


-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- >>> mapM_ (print . showWithData myGraph) $ neighboursOf (VertexId 1) myGraph -- around v
-- (VertexId 2,"w")
-- (VertexId 0,"u")
-- (VertexId 2,"w")
-- (VertexId 3,"x")
-- >>> mapM_ (print . showWithData myGraph) $ neighboursOf (VertexId 3) myGraph -- around x
-- (VertexId 1,"v")
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf     :: VertexIdIn w s -> CPlanarGraph w s v e f -> NonEmptyVector (VertexIdIn w s)
neighboursOf v g = flip tailOf g <$> incomingEdges v g

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic (counterclockwise) order around v.
--
-- >>> nextIncidentEdge (dart 3 "+1") myGraph
-- Dart (Arc 5) +1
-- >>> showWithData myGraph $ nextIncidentEdge (dart 3 "+1") myGraph
-- (Dart (Arc 5) +1,"g+")
-- >>> showWithData myGraph $ nextIncidentEdge (dart 1 "+1") myGraph
-- (Dart (Arc 3) -1,"d-")
--
-- running time: \(O(1)\)
nextIncidentEdge   :: DartId s -> CPlanarGraph w s v e f -> DartId s
nextIncidentEdge d = nextIncidentEdgeFrom (Dart.twin d)

-- | Given a dart d that points into some vertex v, report the previous dart in the
-- cyclic (counterclockwise) order around v.
--
-- >>> prevIncidentEdge (dart 3 "+1") myGraph
-- Dart (Arc 1) -1
-- >>> showWithData myGraph $ prevIncidentEdge (dart 3 "+1") myGraph
-- (Dart (Arc 1) -1,"b-")
--
-- running time: \(O(1)\)
prevIncidentEdge   :: DartId s -> CPlanarGraph w s v e f -> DartId s
prevIncidentEdge d = prevIncidentEdgeFrom (Dart.twin d)


-- | Given a dart d that points away from some vertex v, report the
-- next dart in the cyclic (counterclockwise) order around v.
--
-- >>> nextIncidentEdgeFrom (Dart (Arc 3) Positive) myGraph
-- Dart (Arc 2) -1
-- >>> showWithData myGraph $ nextIncidentEdgeFrom (Dart (Arc 3) Positive) myGraph
-- (Dart (Arc 2) -1,"c-")
-- >>> showWithData myGraph $ nextIncidentEdgeFrom (dart 1 "+1") myGraph
-- (Dart (Arc 0) +1,"a+")
--
-- running time: \(O(1)\)
nextIncidentEdgeFrom     :: DartId s -> CPlanarGraph w s v e f -> DartId s
nextIncidentEdgeFrom d g = let perm  = g^.embedding
                               (i,j) = lookupIdx perm d
                           in next (perm^?!orbits.ix i) j


-- | Given a dart d that points into away from vertex v, report the previous dart in the
-- cyclic (counterclockwise) order around v.
--
-- >>> prevIncidentEdgeFrom (Dart (Arc 3) Positive) myGraph
-- Dart (Arc 4) +1
-- >>> showWithData myGraph $ prevIncidentEdgeFrom (Dart (Arc 3) Positive) myGraph
-- (Dart (Arc 4) +1,"e+")
-- >>> showWithData myGraph $ prevIncidentEdgeFrom (Dart (Arc 1) Positive) myGraph
-- (Dart (Arc 2) +1,"c+")
--
-- running time: \(O(1)\)
prevIncidentEdgeFrom     :: DartId s -> CPlanarGraph w s v e f -> DartId s
prevIncidentEdgeFrom d g = let perm  = g^.embedding
                               (i,j) = lookupIdx perm d
                           in previous (perm^?!orbits.ix i) j


--------------------------------------------------------------------------------
-- * Access data

-- | General interface to accessing vertex data, dart data, and face data.
class HasDataOf g i where
  type DataOf g i
  -- | get the data associated with the value i.
  --
  -- running time: \(O(1)\) to read the data, \(O(n)\) to write it.
  dataOf :: i -> IndexedLens' i g (DataOf g i)

instance HasDataOf (CPlanarGraph w s v e f) (VertexIdIn w s) where
  type DataOf (CPlanarGraph w s v e f) (VertexIdIn w s) = v
  dataOf vi@(VertexId i) = reindexed (const vi) $ vertexData.singular (iix i)

instance HasDataOf (CPlanarGraph w s v e f) (DartId s) where
  type DataOf (CPlanarGraph w s v e f) (DartId s) = e
  dataOf d = reindexed (const d) $ dartData.singular (iix $ fromEnum d)

instance HasDataOf (CPlanarGraph w s v e f) (FaceIdIn w s) where
  type DataOf (CPlanarGraph w s v e f) (FaceIdIn w s) = f
  dataOf fi@(FaceId (VertexId i)) = reindexed (const fi) $ faceData.singular (iix i)

-- | Data corresponding to the endpoints of the dart
--
-- >>> myGraph^.endPointDataOf (Dart (Arc 3) Positive)
-- ("w","v")
endPointDataOf   :: DartId s -> Getter (CPlanarGraph w s v e f) (v,v)
endPointDataOf d = to $ endPointData d


-- | Data corresponding to the endpoints of the dart
--
-- running time: \(O(1)\)
endPointData     :: DartId s -> CPlanarGraph w s v e f -> (v,v)
endPointData d g = let (u,v) = endPoints d g in (g^.dataOf u, g^.dataOf v)


--------------------------------------------------------------------------------
-- * The Dual graph

-- | The dual of this graph
--
-- >>> :{
--  let fromList = V.unsafeFromList
--      answer = fromList [ fromList [dart 0 "-1"]
--                        , fromList [dart 2 "+1",dart 4 "+1",dart 1 "-1",dart 0 "+1"]
--                        , fromList [dart 1 "+1",dart 3 "-1",dart 2 "-1"]
--                        , fromList [dart 4 "-1",dart 3 "+1",dart 5 "+1",dart 5 "-1"]
--                        ]
--  in (computeDual myGraph)^.embedding.orbits == answer
-- :}
-- True
--
-- running time: \(O(n)\).
computeDual :: forall s w v e f. CPlanarGraph w s v e f -> CPlanarGraph (DualOf w) s f e v
computeDual = gcastWith proof computeDual'
  where
    proof :: DualOf (DualOf w) :~: w
    proof = dualDualIdentity

-- | Does the actual work for dualGraph
computeDual'   :: (DualOf (DualOf w) ~ w)
               => CPlanarGraph w s v e f -> CPlanarGraph (DualOf w) s f e v
computeDual' g = dualG
  where
    perm  = g^.embedding
    dualG = CPlanarGraph (cycleRep (elems perm) (apply perm . Dart.twin))
                         (g^.faceData)
                         (g^.dartData)
                         (g^.vertexData)
                         g



--------------------------------------------------------------------------------

-- myGraph :: PlanarGraph Primal () String String String
-- myGraph = planarGraph [ [ (Dart aA Negative, "a-")
--                             , (Dart aC Positive, "c+")
--                             , (Dart aB Positive, "b+")
--                             , (Dart aA Positive, "a+")
--                             ]
--                           , [ (Dart aE Negative, "e-")
--                             , (Dart aB Negative, "b-")
--                             , (Dart aD Negative, "d-")
--                             , (Dart aG Positive, "g+")
--                             ]
--                           , [ (Dart aE Positive, "e+")
--                             , (Dart aD Positive, "d+")
--                             , (Dart aC Negative, "c-")
--                             ]
--                           , [ (Dart aG Negative, "g-")
--                             ]
--                           ]
--           & vertexData .~ V.fromList ["u","v","w","x"]
--           & faceData   .~ V.fromList ["f_3", "f_infty","f_1","f_2"]
--   where
--     (aA:aB:aC:aD:aE:aG:_) = take 6 [Arc 0..]

-- dart i s = Dart (Arc i) (read s)

-- showWithData     :: HasDataOf s i => s -> i -> (i, DataOf s i)
-- showWithData g i = (i, g^.dataOf i)
