{-# LANGUAGE StandaloneDeriving
           , DataKinds
           , TypeOperators
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , TemplateHaskell
           , QuasiQuotes
           , MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , TypeSynonymInstances
           , RankNTypes
           , CPP
           , KindSignatures
           , ScopedTypeVariables
           , FunctionalDependencies
           , UndecidableInstances
           , BangPatterns
           #-}
module Sigym4.Geometry.Types (
    Geometry (..)
  , LineString (..)
  , MultiLineString (..)
  , LinearRing (..)
  , Vertex
  , SqMatrix
  , Point (..)
  , MultiPoint (..)
  , Polygon (..)
  , MultiPolygon (..)
  , Triangle (..)
  , TIN (..)
  , PolyhedralSurface (..)
  , GeometryCollection (..)
  , Feature (..)
  , FeatureCollection (..)
  , VectorSpace (..)
  , HasOffset (..)
  , Pixel (..)
  , Size (..)
  , Offset (..)
  , RowMajor
  , ColumnMajor
  , Extent (..)
  , GeoTransform (..)
  , Raster (..)
  , rasterIndexPixel
  , unsafeRasterIndexPixel
  , rasterIndex
  , unsafeRasterIndex
  , northUpGeoTransform
  , generateMRaster
  , GeoReference (..)
  , mkGeoReference
  , pointOffset
  , unsafePointOffset
  , grScalarSize
  , scalarSize
  , grForward
  , grBackward

  , mkLineString
  , mkLinearRing
  , mkPolygon
  , mkTriangle

  , HasCoordinates (..)
  , polygonRings
  , convertRasterOffsetType

  , eSize
  , rasterSize
  , invertible

  -- lenses & prisms
  , HasGeometry (..)
  , HasVertices (..)
  , HasProperties (..)
  , HasFeatures (..)
  , HasPoints (..)
  , HasLineStrings (..)
  , HasOuterRing (..)
  , HasInnerRings (..)
  , HasPolygons (..)
  , HasTriangles (..)
  , HasGeometries (..)
  , HasVertex (..)

  , featureCollectionFeatures
  , featureGeometry
  , featureProperties

  , _GeoPoint
  , _GeoMultiPoint
  , _GeoLineString
  , _GeoMultiLineString
  , _GeoPolygon
  , _GeoMultiPolygon
  , _GeoTriangle
  , _GeoPolyhedralSurface
  , _GeoTIN
  , _GeoCollection

  -- re-exports
  , KnownNat
  , module V1
  , module V2
  , module V3
  , module V4
  , module VN
  , module Linear.Epsilon
  , module SpatialReference
) where

import Prelude hiding (product)
import Control.Lens hiding (coerce)
import Data.Coerce (coerce)
import Data.Distributive (Distributive)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(rnf))
import Control.Exception (assert)
import Control.Monad.Primitive (PrimMonad(..))
import qualified Data.Semigroup as SG
import Data.Foldable (product)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Foreign.Storable (Storable)
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Language.Haskell.TH.Syntax
import Linear.V1 as V1
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.V4 as V4 hiding (vector, point)
import Linear.Epsilon
import Linear.V as VN hiding (dim, Size)
import Linear.Matrix ((!*), (*!), inv22, inv33, inv44, det22, det33, det44)
import Linear.Metric (Metric)
import GHC.TypeLits
import SpatialReference

-- | A vertex
type Vertex v = v Double

-- | A square Matrix
type SqMatrix v = v (Vertex v)

-- | A vector space
class ( KnownNat (VsDim v), Num (Vertex v), Fractional (Vertex v)
      , Show (Vertex v), Eq (Vertex v), Ord (Vertex v), Epsilon (Vertex v)
      , U.Unbox (Vertex v), NFData (Vertex v), NFData (SqMatrix v)
      , NFData (v Int) , Show (v Int), Eq (v Int), Ord (v Int)
      , Num (SqMatrix v), Show (SqMatrix v), Eq (SqMatrix v), Ord (SqMatrix v)
      , Metric v, Applicative v, Foldable v, Traversable v, Distributive v)
  => VectorSpace (v :: * -> *) where
    type VsDim v :: Nat

    inv :: SqMatrix v -> SqMatrix v
    det :: SqMatrix v -> Double

    dim :: Proxy v -> Int
    dim = const (reflectDim (Proxy :: Proxy (VsDim v)))
    {-# INLINE dim #-}

    coords :: v a -> [a]
    fromCoords :: [a] -> Maybe (v a)

    unsafeFromCoords :: [a] -> v a
    unsafeFromCoords = fromMaybe (error "unsafeFromCoords") . fromCoords
    {-# INLINE CONLIKE [1] unsafeFromCoords #-}

    liftTExp :: Lift a => v a -> Q (TExp (v a))

    {-# MINIMAL inv, det, coords , fromCoords, liftTExp #-}


{-# RULES

"unsafeFromCoords/V2" forall u v.
  unsafeFromCoords (u:v:[]) = V2 u v

"unsafeFromCoords/V3" forall u v z.
      unsafeFromCoords (u:v:z:[]) = V3 u v z

"unsafeFromCoords/V4"
 forall u v z w.
        unsafeFromCoords (u:v:z:w:[]) = V4 u v z w
  #-}


invertible :: VectorSpace v => SqMatrix v -> Bool
invertible = not . nearZero . det
{-# INLINE invertible #-}

instance VectorSpace V1 where
    type VsDim V1   = 1
    inv             = (pure 1 /)
    det (V1 (V1 u)) = u
    coords (V1 u)   = [u]
    fromCoords [u]  = Just (V1 u)
    fromCoords _    = Nothing
    liftTExp (V1 u) = [|| V1 u ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}

instance VectorSpace V2 where
    type VsDim V2 = 2
    inv = inv22
    det = det22
    coords (V2 u v)   = [u, v]
    fromCoords [u, v] = Just (V2 u v)
    fromCoords _      = Nothing
    liftTExp (V2 u v) = [|| V2 u v ||]

    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


instance VectorSpace V3 where
    type VsDim V3 = 3
    inv = inv33
    det = det33
    coords (V3 u v z)    = [u, v, z]
    fromCoords [u, v, z] = Just (V3 u v z)
    fromCoords _         = Nothing
    liftTExp (V3 u v z)  = [|| V3 u v z ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


instance VectorSpace V4 where
    type VsDim V4 = 4
    inv = inv44
    det = det44
    coords (V4 u v z w)     = [u, v, z, w]
    fromCoords [u, v, z, w] = Just (V4 u v z w)
    fromCoords _            = Nothing
    liftTExp (V4 u v z w)   = [|| V4 u v z w ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


instance KnownNat n => VectorSpace (V n) where
    type VsDim (V n) = n
    inv = error ("inv not implemented for V " ++ show (dim (Proxy :: Proxy (V n))))
    det = error ("det not implemented for V " ++ show (dim (Proxy :: Proxy (V n))))
    coords           = G.toList . toVector
    fromCoords       = fromVector . G.fromList
    liftTExp (V n)   = let l = G.toList n
                           d = dim (Proxy :: Proxy (V n))
                       in [|| V (G.fromListN d l) ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


newtype Offset (t :: OffsetType) = Offset {unOff :: Int}
  deriving (Eq, Show, Ord, Num)

data OffsetType = RowMajor | ColumnMajor

type RowMajor = 'RowMajor
type ColumnMajor = 'ColumnMajor

class VectorSpace v => HasOffset v (t :: OffsetType) where
    toOffset   :: Size v -> Pixel v -> Maybe (Offset t)
    fromOffset :: Size v -> Offset t -> Maybe (Pixel v)
    unsafeToOffset :: Size v -> Pixel v -> Offset t
    unsafeFromOffset :: Size v -> Offset t -> Pixel v

instance HasOffset V2 RowMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V2 sx sy = unSize s
            V2 px py = fmap truncate $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p = Offset off
      where V2 sx sy = unSize s
            V2 px py = fmap truncate $ unPx p
            off      = assert (0<=px && px<sx && 0<=py && py<sy) (py * sx + px)
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V2 sx sy)) o@(Offset o')
      | 0<=o' && o'<sx*sy = Just (unsafeFromOffset s o)
      | otherwise        = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V2 sx sy)) (Offset o)
      = Pixel (V2 (fromIntegral px) (fromIntegral py))
      where (py,px) =  assert (0<=o && o<(sx*sy)) (o `divMod` sx)
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V2 ColumnMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V2 sx sy = unSize s
            V2 px py = fmap truncate $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p  = Offset off
      where V2 sx sy  = unSize s
            V2 px py = fmap truncate $ unPx p
            off      = assert (0<=px && px<sx && 0<=py && py<sy) (px * sy + py)
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V2 sx sy)) o@(Offset o')
      | 0<=o' && o'<sx*sy = Just (unsafeFromOffset  s o)
      | otherwise        = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V2 sx sy)) (Offset o)
      = Pixel (V2 (fromIntegral px) (fromIntegral py))
      where (px,py) = assert (0<=o && o<(sx*sy)) (o `divMod` sy)
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V3 RowMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy
      , 0<=pz && pz < sz = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V3 sx sy sz = unSize s
            V3 px py pz = fmap truncate $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p  = Offset $ (pz * sx * sy) + (py * sx) + px
      where V3 sx sy _  = unSize s
            V3 px py pz = fmap truncate $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V3 sx sy sz)) o@(Offset o')
      | 0<=o' && o'<sx*sy*sz = Just (unsafeFromOffset  s o)
      | otherwise            = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V3 sx sy _)) (Offset o)
      = Pixel (V3 (fromIntegral px) (fromIntegral py) (fromIntegral pz))
      where (pz, r) =  o `divMod` (sx*sy)
            (py,px) =  r `divMod` sx
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V3 ColumnMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy
      , 0<=pz && pz < sz = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V3 sx sy sz = unSize s
            V3 px py pz = fmap truncate $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p = Offset $ (px * sz * sy) + (py * sz) + pz
      where V3 _  sy sz = unSize s
            V3 px py pz = fmap truncate $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V3 sx sy sz)) o@(Offset o')
      | 0<=o' && o'<sx*sy*sz = Just (unsafeFromOffset  s o)
      | otherwise            = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V3 _ sy sz)) (Offset o)
      = Pixel (V3 (fromIntegral px) (fromIntegral py) (fromIntegral pz))
      where (px, r) =  o `divMod` (sz*sy)
            (py,pz) =  r `divMod` sz
    {-# INLINE unsafeFromOffset #-}

-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v crs = Extent {eMin :: !(Vertex v), eMax :: !(Vertex v)}
deriving instance VectorSpace v => Eq (Extent v crs)
deriving instance VectorSpace v => Show (Extent v crs)

instance NFData (Vertex v) => NFData (Extent v crs) where
  rnf (Extent lo hi) = rnf lo `seq` rnf hi `seq` ()

instance Hashable (Vertex v) => Hashable (Extent v crs) where
  hashWithSalt i (Extent l h) = hashWithSalt i l + hashWithSalt i h

eSize :: VectorSpace v => Extent v crs -> Vertex v
eSize e = eMax e - eMin e
{-# INLINE eSize #-}

instance VectorSpace v => SG.Semigroup (Extent v crs) where
    Extent a0 a1 <> Extent b0 b1
        = Extent (min <$> a0 <*> b0) (max <$> a1 <*> b1)
    {-# INLINE (<>) #-}

-- | A pixel is a newtype around a vertex
newtype Pixel v = Pixel {unPx :: Vertex v}
deriving instance VectorSpace v => Show (Pixel v)
deriving instance VectorSpace v => Eq (Pixel v)

newtype Size v = Size {unSize :: v Int}
deriving instance VectorSpace v => Eq (Size v)
deriving instance VectorSpace v => Ord(Size v)
deriving instance VectorSpace v => Show (Size v)
deriving instance VectorSpace v => NFData (Size v)

scalarSize :: VectorSpace v => Size v -> Int
scalarSize = product . unSize
{-# INLINE scalarSize #-}


-- A GeoTransform defines how we translate from geographic 'Vertex'es to
-- 'Pixel' coordinates and back. gtMatrix *must* be invertible so smart
-- constructors are provided
data GeoTransform v crs = GeoTransform
      { gtMatrix :: !(SqMatrix v)
      , gtOrigin :: !(Vertex v)
      }
deriving instance VectorSpace v => Ord (GeoTransform v crs)
deriving instance VectorSpace v => Eq (GeoTransform v crs)
deriving instance VectorSpace v => Show (GeoTransform v crs)

instance VectorSpace v => NFData (GeoTransform v crs) where
  rnf (GeoTransform gr s) = rnf gr `seq` rnf s `seq` ()

northUpGeoTransform :: Extent V2 crs -> Size V2
                    -> Either String (GeoTransform V2 crs)
northUpGeoTransform e s
  | not isValidBox   = Left "northUpGeoTransform: invalid extent"
  | not isValidSize  = Left "northUpGeoTransform: invalid size"
  | otherwise        = Right $ GeoTransform matrix origin
  where
    isValidBox  = fmap (> 0) (eMax e - eMin e)  == pure True
    isValidSize = fmap (> 0) s'                 == pure True
    V2 x0 _     = eMin e
    V2 _ y1     = eMax e
    origin      = V2 x0 y1
    s'          = fmap fromIntegral $ unSize s
    V2 dx dy    = (eMax e - eMin e)/s'
    matrix      = V2 (V2 dx 0) (V2 0 (-dy))

gtForward :: VectorSpace v => GeoTransform v crs -> Point v crs -> Pixel v
gtForward gt (Point v) = Pixel $ m !* (v-v0)
  where v0  = gtOrigin gt
        m
#if ASSERTS
          | not (invertible (gtMatrix gt)) = error "gtMatrix is not invertible"
#endif
          | otherwise                      = inv (gtMatrix gt)


gtBackward :: VectorSpace v => GeoTransform v crs -> Pixel v -> Point v crs
gtBackward gt p = Point $ v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v crs = GeoReference
      { grTransform :: !(GeoTransform v crs)
      , grSize      :: !(Size v)
      }
deriving instance VectorSpace v => Eq (GeoReference v crs)
deriving instance VectorSpace v => Ord (GeoReference v crs)
deriving instance VectorSpace v => Show (GeoReference v crs)

instance VectorSpace v => NFData (GeoReference v crs) where
  rnf (GeoReference gr s) = rnf gr `seq` rnf s `seq` ()

grScalarSize :: VectorSpace v => GeoReference v crs -> Int
grScalarSize = scalarSize . grSize
{-# INLINE grScalarSize #-}


pointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v crs -> Point v crs -> Maybe (Offset t)
pointOffset gr =  toOffset (grSize gr) . grForward gr
{-# INLINE pointOffset #-}

unsafePointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v crs -> Point v crs -> Offset t
unsafePointOffset gr =  unsafeToOffset (grSize gr) . grForward gr
{-# INLINE unsafePointOffset #-}


grForward :: VectorSpace v => GeoReference v crs -> Point v crs -> Pixel v
grForward gr = gtForward (grTransform gr)
{-# INLINE grForward #-}

grBackward :: VectorSpace v => GeoReference v crs -> Pixel v -> Point v crs
grBackward gr = gtBackward (grTransform gr)
{-# INLINE grBackward #-}


mkGeoReference :: Extent V2 crs -> Size V2 -> Either String (GeoReference V2 crs)
mkGeoReference e s = fmap (\gt -> GeoReference gt s) (northUpGeoTransform e s)

newtype Point v crs = Point {_pointVertex:: Vertex v}
deriving instance VectorSpace v          => Show (Point v crs)
deriving instance VectorSpace v          => Eq (Point v crs)
deriving instance VectorSpace v          => Ord (Point v crs)
deriving instance NFData      (Vertex v) => NFData (Point v crs)
deriving instance Hashable    (Vertex v) => Hashable (Point v crs)
deriving instance Storable    (Vertex v) => Storable (Point v crs)

class HasVertex o a | o->a where
  vertex :: Lens' o a

class VectorSpace v => HasVertices a v | a->v where
  vertices :: Traversal a a (Vertex v) (Vertex v)



instance HasVertex (Point v crs) (Vertex v) where
  vertex = lens coerce (const coerce)
  {-# INLINE vertex #-}

instance HasVertex (WithSomeCrs (Point v)) (Vertex v) where
  vertex = lens (\(WithSomeCrs (Point v)) -> v)
                (\(WithSomeCrs p) v -> WithSomeCrs (p&vertex.~v))
  {-# INLINE vertex #-}


derivingUnbox "Point"
    [t| forall v crs. VectorSpace v => Point v crs -> Vertex v |]
    [| coerce |]
    [| coerce |]

derivingUnbox "Extent"
    [t| forall v crs. VectorSpace v => Extent v crs -> (Vertex v,Vertex v) |]
    [| \(Extent e0 e1) -> (e0,e1) |]
    [| \(e0,e1) -> Extent e0 e1 |]

derivingUnbox "Pixel"
    [t| forall v. VectorSpace v => Pixel v -> Vertex v |]
    [| coerce |]
    [| coerce |]

derivingUnbox "Offset"
    [t| forall t. Offset (t :: OffsetType) -> Int |]
    [| coerce |]
    [| coerce |]

newtype MultiPoint v crs = MultiPoint {
    _multiPointPoints :: U.Vector (Point v crs)
} deriving (Eq, Show)
makeFields ''MultiPoint

deriving instance VectorSpace v => NFData (MultiPoint v crs)

newtype LinearRing v crs
  = LinearRing {_linearRingPoints :: U.Vector (Point v crs)}
  deriving (Eq, Show)
makeFields ''LinearRing

deriving instance VectorSpace v => NFData (LinearRing v crs)

newtype LineString v crs
  = LineString {_lineStringPoints :: U.Vector (Point v crs)}
  deriving (Eq, Show)
makeFields ''LineString

deriving instance VectorSpace v => NFData (LineString v crs)

newtype MultiLineString v crs = MultiLineString {
    _multiLineStringLineStrings :: V.Vector (LineString v crs)
} deriving (Eq, Show)
makeFields ''MultiLineString

deriving instance VectorSpace v => NFData (MultiLineString v crs)

data Triangle v crs = Triangle !(Point v crs) !(Point v crs) !(Point v crs)
    deriving (Eq, Show)

instance VectorSpace v => NFData (Triangle v crs) where
  rnf (Triangle a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

derivingUnbox "Triangle"
    [t| forall v crs. VectorSpace v => Triangle v crs -> (Point v crs, Point v crs, Point v crs) |]
    [| \(Triangle a b c) -> (a, b, c) |]
    [| \(a, b, c) -> Triangle a b c|]

data Polygon v crs = Polygon {
    _polygonOuterRing  :: !(LinearRing v crs)
  , _polygonInnerRings :: !(V.Vector (LinearRing v crs))
} deriving (Eq, Show)
makeFields ''Polygon

instance VectorSpace v => NFData (Polygon v crs) where
  rnf (Polygon o r) = rnf o `seq` rnf r `seq` ()

newtype MultiPolygon v crs = MultiPolygon {
    _multiPolygonPolygons :: V.Vector (Polygon v crs)
} deriving (Eq, Show)
makeFields ''MultiPolygon

deriving instance VectorSpace v => NFData (MultiPolygon v crs)

newtype PolyhedralSurface v crs = PolyhedralSurface {
    _polyhedralSurfacePolygons :: V.Vector (Polygon v crs)
} deriving (Eq, Show)
makeFields ''PolyhedralSurface

deriving instance VectorSpace v => NFData (PolyhedralSurface v crs)

newtype TIN v crs = TIN {
    _tINTriangles :: U.Vector (Triangle v crs)
} deriving (Eq, Show)
makeFields ''TIN

deriving instance VectorSpace v => NFData (TIN v crs)

data Geometry v crs
    = GeoPoint !(Point v crs)
    | GeoMultiPoint !(MultiPoint v crs)
    | GeoLineString !(LineString v crs)
    | GeoMultiLineString !(MultiLineString v crs)
    | GeoPolygon !(Polygon v crs)
    | GeoMultiPolygon !(MultiPolygon v crs)
    | GeoTriangle !(Triangle v crs)
    | GeoPolyhedralSurface !(PolyhedralSurface v crs)
    | GeoTIN !(TIN v crs)
    | GeoCollection !(GeometryCollection v crs)
    deriving (Eq, Show)

instance VectorSpace v => NFData (Geometry v crs) where
  rnf (GeoPoint g)             = rnf g
  rnf (GeoMultiPoint g)        = rnf g
  rnf (GeoLineString g)        = rnf g
  rnf (GeoMultiLineString g)   = rnf g
  rnf (GeoPolygon g)           = rnf g
  rnf (GeoMultiPolygon g)      = rnf g
  rnf (GeoTriangle g)          = rnf g
  rnf (GeoPolyhedralSurface g) = rnf g
  rnf (GeoTIN g)               = rnf g
  rnf (GeoCollection g)        = rnf g

newtype GeometryCollection v crs = GeometryCollection {
    _geometryCollectionGeometries :: V.Vector (Geometry v crs)
} deriving (Eq, Show)
makeFields ''GeometryCollection
makePrisms ''Geometry

deriving instance VectorSpace v => NFData (GeometryCollection v crs)


mkLineString :: VectorSpace v => [Point v crs] -> Maybe (LineString v crs)
mkLineString ls
  | U.length v >= 2 = Just $ LineString v
  | otherwise = Nothing
  where v = U.fromList ls

mkLinearRing :: VectorSpace v => [Point v crs] -> Maybe (LinearRing v crs)
mkLinearRing ls
  | U.length v >= 4, U.last v == U.head v = Just $ LinearRing v
  | otherwise = Nothing
  where v = U.fromList ls

mkPolygon :: [LinearRing v crs] -> Maybe (Polygon v crs)
mkPolygon (oRing:rings_) = Just $ Polygon oRing $ V.fromList rings_
mkPolygon _ = Nothing

mkTriangle :: VectorSpace v
  => Point v crs -> Point v crs -> Point v crs -> Maybe (Triangle v crs)
mkTriangle a b c | a/=b, b/=c, a/=c = Just $ Triangle a b c
                 | otherwise = Nothing

class HasCoordinates o a | o -> a where
  coordinates :: o -> a

instance VectorSpace v => HasCoordinates (Point v crs) [Double] where
  coordinates = views vertex coords
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (MultiPoint v crs) [[Double]] where
  coordinates = views points (G.toList . V.map coordinates . G.convert)
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (LineString v crs) [[Double]] where
  coordinates = views points coordinates
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (MultiLineString v crs) [[[Double]]] where
  coordinates = views lineStrings (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (LinearRing v crs) [[Double]] where
  coordinates = views points coordinates
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (Polygon v crs) [[[Double]]] where
  coordinates = V.toList . V.map coordinates . polygonRings
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (MultiPolygon v crs) [[[[Double]]]] where
  coordinates = views polygons (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (PolyhedralSurface v crs) [[[[Double]]]] where
  coordinates = views polygons (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

polygonRings :: Polygon v crs -> V.Vector (LinearRing v crs)
polygonRings (Polygon ir rs) = V.cons ir rs
{-# INLINE polygonRings #-}

instance VectorSpace v => HasCoordinates (Triangle v crs) [[Double]] where
  coordinates (Triangle a b c) = map coordinates [a, b, c, a]
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (TIN v crs) [[[Double]]] where
  coordinates = views triangles (V.toList . V.map coordinates . G.convert)
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (U.Vector (Point v crs)) [[Double]] where
  coordinates = V.toList . V.map coordinates . V.convert
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (V.Vector (Point v crs)) [[Double]] where
  coordinates = V.toList . V.map coordinates
  {-# INLINE coordinates #-}




-- | A feature of 'GeometryType' t, vertex type 'v' and associated data 'd'
data Feature (g :: * -> *) d crs = Feature {
    _featureGeometry   :: g crs
  , _featureProperties :: d
  } deriving (Eq, Show)
makeFields ''Feature
makeLenses ''Feature


instance HasProperties (WithSomeCrs (Feature g d)) d where
  properties = lens (\(WithSomeCrs f) -> f^.properties)
                    (\(WithSomeCrs f) p -> WithSomeCrs (f & properties .~ p))


instance HasGeometry (WithSomeCrs (Feature g d)) (WithSomeCrs g) where
  geometry = lens (\(WithSomeCrs f) -> WithSomeCrs (f^.geometry))
                  (\(WithSomeCrs (Feature _ p)) (WithSomeCrs g) ->
                      WithSomeCrs (Feature g p))

instance (NFData d, NFData (g crs)) => NFData (Feature g d crs) where
  rnf (Feature g v) = rnf g `seq` rnf v `seq` ()


derivingUnbox "UnboxFeature"
    [t| forall g crs a. (U.Unbox a, U.Unbox (g crs))
        => Feature g a crs -> (g crs, a) |]
    [| \(Feature p v) -> (p,v) |]
    [| \(p,v) -> Feature p v|]

newtype FeatureCollection (g :: * -> *) d crs
  = FeatureCollection {
    _featureCollectionFeatures :: [Feature g d crs]
  } deriving (Eq, Show)
makeFields ''FeatureCollection
makeLenses ''FeatureCollection


instance Monoid (FeatureCollection g d crs) where
    mempty = FeatureCollection mempty
    mappend = (<>)

instance Semigroup (FeatureCollection g d crs) where
  FeatureCollection as <> FeatureCollection bs = FeatureCollection (as <> bs)

instance VectorSpace v => HasVertices (FeatureCollection (Point v) d crs) v
  where vertices = features.traverse.geometry.vertex

data Raster vs (t :: OffsetType) crs v a
  = Raster {
      rGeoReference :: !(GeoReference vs crs)
    , rData         :: !(v a)
    } deriving (Eq, Show)

instance (NFData (v a), VectorSpace vs) => NFData (Raster vs t crs v a) where
  rnf (Raster a b) = rnf a `seq` rnf b `seq` ()


rasterSize :: VectorSpace v => Extent v crs -> Vertex v -> Size v
rasterSize e pxSize = Size $ fmap ceiling (eSize e / pxSize)
{-# INLINE rasterSize #-}

generateMRaster
  :: forall m v a vs t crs. (PrimMonad m, GM.MVector v a, HasOffset vs t)
  => GeoReference vs crs
  -> (Point vs crs -> a)
  -> m (Raster vs t crs (v (PrimState m)) a)
generateMRaster geoRef fun = do
  v <- GM.new sz
  loop v (sz-1)
  return (Raster geoRef v)
  where
    sz = grScalarSize geoRef
    loop v !i | i<0 = return ()
    loop v !i =
      let !px = unsafeFromOffset (grSize geoRef) (Offset i :: Offset t)
          !p  = grBackward geoRef px
      in GM.unsafeWrite v i (fun p) >> loop v (i-1)
{-# INLINE generateMRaster #-}

rasterIndex
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Point vs crs -> Maybe Int
rasterIndex r p = fmap unOff offset
  where
    offset = pointOffset (rGeoReference r) p :: Maybe (Offset t)
{-# INLINE rasterIndex #-}

unsafeRasterIndex
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Point vs crs -> Int
unsafeRasterIndex r p = unOff offset
  where
    offset = unsafePointOffset (rGeoReference r) p :: Offset t
{-# INLINE unsafeRasterIndex #-}

rasterIndexPixel
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Pixel vs -> Maybe Int
rasterIndexPixel r px = fmap unOff offset
  where
    offset = toOffset (grSize (rGeoReference r)) px :: Maybe  (Offset t)
{-# INLINE rasterIndexPixel #-}

unsafeRasterIndexPixel
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Pixel vs -> Int
unsafeRasterIndexPixel r px = unOff offset
  where
    offset = unsafeToOffset (grSize (rGeoReference r)) px :: Offset t
{-# INLINE unsafeRasterIndexPixel #-}


convertRasterOffsetType
  :: forall vs t1 t2 crs v a. (G.Vector v a, HasOffset vs t1, HasOffset vs t2)
  => Raster vs t1 crs v a -> Raster vs t2 crs v a
convertRasterOffsetType r = r {rData = G.generate n go}
  where go i = let px        = unsafeFromOffset s (Offset i :: Offset t2)
                   Offset i' = unsafeToOffset s px :: Offset t1
               in rd `G.unsafeIndex` i'
        s = grSize (rGeoReference r)
        rd = rData r
        n  = G.length rd
