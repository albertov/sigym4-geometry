{-# LANGUAGE StandaloneDeriving
           , DataKinds
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , TemplateHaskell
           , MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , TypeSynonymInstances
           , RankNTypes
           , CPP
           , KindSignatures
           , DeriveFunctor
           , ScopedTypeVariables
           #-}
module Sigym4.Geometry.Types (
    Geometry (..)
  , LineString (..)
  , MultiLineString (..)
  , LinearRing (..)
  , Vertex
  , Point (..)
  , MultiPoint (..)
  , Polygon (..)
  , MultiPolygon (..)
  , Triangle (..)
  , TIN (..)
  , PolyhedralSurface (..)
  , GeometryCollection (..)
  , Feature
  , FeatureCollection
  , FeatureT (..)
  , FeatureCollectionT (..)
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
  , indexRaster
  , northUpGeoTransform
  , GeoReference (..)
  , mkGeoReference
  , pointOffset
  , grScalarSize
  , scalarSize
  , grForward
  , grBackward

  , mkLineString
  , mkLinearRing
  , mkPolygon
  , mkTriangle

  , pointCoordinates
  , lineStringCoordinates
  , polygonCoordinates
  , polygonRings
  , triangleCoordinates
  , convertRasterOffsetType

  , gSrid
  , hasSrid
  , withSrid

  , eSize

  -- lenses & prisms
  , pVertex
  , fGeom
  , fData
  , fcFeatures
  , mpPoints
  , lrPoints
  , lsPoints
  , mlLineStrings
  , pOuterRing
  , pRings
  , mpPolygons
  , psPolygons
  , tinTriangles
  , gcGeometries

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

  , NoSrid

  -- re-exports
  , KnownNat
  , Nat
  , module V2
  , module V3
) where

import Prelude hiding (product)
#if MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#else
import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
#endif
import Control.Lens
import Data.Proxy (Proxy(..))
import Data.Maybe (fromMaybe)
import qualified Data.Semigroup as SG
import Data.Foldable (product)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.Matrix ((!*), (*!), inv22, inv33)
import Linear.Metric (Metric)
import GHC.TypeLits

-- | A vertex
type Vertex v = v Double

-- | A square Matrix
type SqMatrix v = v (Vertex v)

-- | A vector space
class ( Num (Vertex v), Fractional (Vertex v)
      , Show (Vertex v), Eq (Vertex v), U.Unbox (Vertex v)
      , Show (v Int), Eq (v Int), Eq (v Bool)
      , Num (SqMatrix v), Show (SqMatrix v), Eq (SqMatrix v)
      , Metric v, Applicative v, Foldable v)
  => VectorSpace v where
    inv :: SqMatrix v -> Maybe (SqMatrix v)
    dim :: Proxy v -> Int
    coords :: Vertex v -> [Double]
    fromCoords :: [Double] -> Maybe (Vertex v)

instance VectorSpace V2 where
    inv = inv22
    dim _ = 2
    coords (V2 u v) = [u, v]
    fromCoords [u, v] = Just $ V2 u v
    fromCoords _ = Nothing
    {-# INLINE dim #-}
    {-# INLINE fromCoords #-}
    {-# INLINE coords #-}

instance VectorSpace V3 where
    inv = inv33
    dim _ = 3
    coords (V3 u v z) = [u, v, z]
    fromCoords [u, v, z] = Just $ V3 u v z
    fromCoords _ = Nothing
    {-# INLINE dim #-}
    {-# INLINE fromCoords #-}
    {-# INLINE coords #-}


newtype Offset (t :: OffsetType) = Offset {unOff :: Int}
  deriving (Eq, Show, Ord, Num)

data OffsetType = RowMajor | ColumnMajor

type RowMajor = 'RowMajor
type ColumnMajor = 'ColumnMajor

class HasOffset v (t :: OffsetType) where
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
            V2 px py = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p = Offset $ py * sx + px
      where V2 sx _  = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V2 sx sy)) o@(Offset o')
      | 0<=o' && o'<sx*sy = Just (unsafeFromOffset  s o)
      | otherwise        = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V2 sx _)) (Offset o)
      = Pixel (V2 (fromIntegral px) (fromIntegral py))
      where (py,px) =  o `divMod` sx
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V2 ColumnMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V2 sx sy = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p  = Offset $ px * sy + py
      where V2 _ sy  = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V2 sx sy)) o@(Offset o')
      | 0<=o' && o'<sx*sy = Just (unsafeFromOffset  s o)
      | otherwise        = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V2 _ sy)) (Offset o)
      = Pixel (V2 (fromIntegral px) (fromIntegral py))
      where (px,py) =  o `divMod` sy
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V3 RowMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy
      , 0<=pz && pz < sz = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V3 sx sy sz = unSize s
            V3 px py pz = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p  = Offset $ (pz * sx * sy) + (py * sx) + px
      where V3 sx sy _  = unSize s
            V3 px py pz = fmap floor $ unPx p
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
            V3 px py pz = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p = Offset $ (px * sz * sy) + (py * sz) + pz
      where V3 _  sy sz = unSize s
            V3 px py pz = fmap floor $ unPx p
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

type NoSrid = 0


-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v (srid :: Nat) = Extent {eMin :: !(Vertex v), eMax :: !(Vertex v)}
deriving instance VectorSpace v => Eq (Extent v srid)
deriving instance VectorSpace v => Show (Extent v srid)

eSize :: VectorSpace v => Extent v srid -> Vertex v
eSize e = eMax e - eMin e

instance VectorSpace v => SG.Semigroup (Extent v srid) where
    Extent a0 a1 <> Extent b0 b1
        = Extent (min <$> a0 <*> b0) (max <$> a1 <*> b1)

-- | A pixel is a newtype around a vertex
newtype Pixel v = Pixel {unPx :: Vertex v}
deriving instance VectorSpace v => Show (Pixel v)
deriving instance VectorSpace v => Eq (Pixel v)

newtype Size v = Size {unSize :: v Int}
deriving instance VectorSpace v => Eq (Size v)
deriving instance VectorSpace v => Show (Size v)

scalarSize :: VectorSpace v => Size v -> Int
scalarSize = product . unSize


-- A GeoTransform defines how we translate from geographic 'Vertex'es to
-- 'Pixel' coordinates and back. gtMatrix *must* be inversible so smart
-- constructors are provided
data GeoTransform v (srid :: Nat) = GeoTransform 
      { gtMatrix :: !(SqMatrix v)
      , gtOrigin :: !(Vertex v)
      }
deriving instance VectorSpace v => Eq (GeoTransform v srid)
deriving instance VectorSpace v => Show (GeoTransform v srid)

northUpGeoTransform :: Extent V2 srid -> Size V2
                    -> Either String (GeoTransform V2 srid)
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

gtForward :: VectorSpace v => GeoTransform v srid -> Point v srid -> Pixel v
gtForward gt (Point v) = Pixel $ m !* (v-v0)
  where m   = fromMaybe (error "gtForward. non-inversible matrix")
                        (inv $ gtMatrix gt)
        v0  = gtOrigin gt


gtBackward :: VectorSpace v => GeoTransform v srid -> Pixel v -> Point v srid
gtBackward gt p = Point $ v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v srid = GeoReference 
      { grTransform :: GeoTransform v srid
      , grSize      :: Size v
      }
deriving instance VectorSpace v => Eq (GeoReference v srid)
deriving instance VectorSpace v => Show (GeoReference v srid)


grScalarSize :: VectorSpace v => GeoReference v srid -> Int
grScalarSize = scalarSize . grSize


pointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v srid -> Point v srid -> Maybe (Offset t)
pointOffset gr =  toOffset (grSize gr) . grForward gr
{-# INLINEABLE pointOffset #-}

grForward :: VectorSpace v => GeoReference v srid -> Point v srid -> Pixel v
grForward gr = gtForward (grTransform gr)
{-# INLINE grForward #-}

grBackward :: VectorSpace v => GeoReference v srid -> Pixel v -> Point v srid
grBackward gr = gtBackward (grTransform gr)
{-# INLINE grBackward #-}


mkGeoReference :: Extent V2 srid -> Size V2 -> Either String (GeoReference V2 srid)
mkGeoReference e s = fmap (\gt -> GeoReference gt s) (northUpGeoTransform e s)

newtype Point v (srid :: Nat) = Point {_pVertex:: Vertex v}
deriving instance VectorSpace v => Show (Point v srid)
deriving instance VectorSpace v => Eq (Point v srid)

pVertex :: VectorSpace v => Lens' (Point v srid) (Vertex v)
pVertex = lens _pVertex (\point v -> point { _pVertex = v })
{-# INLINE pVertex #-}

derivingUnbox "Point"
    [t| forall v srid. VectorSpace v => Point v srid -> Vertex v |]
    [| \(Point v) -> v |]
    [| \v -> Point v|]

derivingUnbox "Pixel"
    [t| forall v. VectorSpace v => Pixel v -> Vertex v |]
    [| \(Pixel v) -> v |]
    [| \v -> Pixel v|]

derivingUnbox "Offset"
    [t| forall t. Offset (t :: OffsetType) -> Int |]
    [| \(Offset o) -> o |]
    [| \o -> Offset o|]

newtype MultiPoint v srid = MultiPoint {
    _mpPoints :: V.Vector (Point v srid)
} deriving (Eq, Show)
makeLenses ''MultiPoint

newtype LinearRing v srid = LinearRing {_lrPoints :: U.Vector (Point v srid)}
    deriving (Eq, Show)
makeLenses ''LinearRing

newtype LineString v srid = LineString {_lsPoints :: U.Vector (Point v srid)}
    deriving (Eq, Show)
makeLenses ''LineString

newtype MultiLineString v srid = MultiLineString {
    _mlLineStrings :: V.Vector (LineString v srid)
} deriving (Eq, Show)
makeLenses ''MultiLineString

data Triangle v srid = Triangle !(Point v srid) !(Point v srid) !(Point v srid)
    deriving (Eq, Show)

derivingUnbox "Triangle"
    [t| forall v srid. VectorSpace v => Triangle v srid -> (Point v srid, Point v srid, Point v srid) |]
    [| \(Triangle a b c) -> (a, b, c) |]
    [| \(a, b, c) -> Triangle a b c|]

data Polygon v srid = Polygon {
    _pOuterRing :: LinearRing v srid
  , _pRings     :: V.Vector (LinearRing v srid) 
} deriving (Eq, Show)
makeLenses ''Polygon

newtype MultiPolygon v srid = MultiPolygon {
    _mpPolygons :: V.Vector (Polygon v srid)
} deriving (Eq, Show)
makeLenses ''MultiPolygon

newtype PolyhedralSurface v srid = PolyhedralSurface {
    _psPolygons :: V.Vector (Polygon v srid)
} deriving (Eq, Show)
makeLenses ''PolyhedralSurface

newtype TIN v srid = TIN {
    _tinTriangles :: U.Vector (Triangle v srid)
} deriving (Eq, Show)
makeLenses ''TIN

data Geometry v (srid::Nat)
    = GeoPoint (Point v srid)
    | GeoMultiPoint (MultiPoint v srid)
    | GeoLineString (LineString v srid)
    | GeoMultiLineString (MultiLineString v srid)
    | GeoPolygon (Polygon v srid)
    | GeoMultiPolygon (MultiPolygon v srid)
    | GeoTriangle (Triangle v srid)
    | GeoPolyhedralSurface (PolyhedralSurface v srid)
    | GeoTIN (TIN v srid)
    | GeoCollection (GeometryCollection v srid)
    deriving (Eq, Show)

newtype GeometryCollection v srid = GeometryCollection {
    _gcGeometries :: V.Vector (Geometry v srid)
} deriving (Eq, Show)
makeLenses ''GeometryCollection
makePrisms ''Geometry


gSrid :: KnownNat srid => proxy srid -> Integer
gSrid = natVal

withSrid
  :: Integer
  -> (forall srid. KnownNat srid => Proxy srid -> a)
  -> Maybe a
withSrid srid f
  = case someNatVal srid of
      Just (SomeNat a) -> Just (f a)
      Nothing          -> Nothing

hasSrid :: KnownNat srid => Geometry v srid -> Bool
hasSrid = (/= 0) . gSrid

mkLineString :: VectorSpace v => [Point v srid] -> Maybe (LineString v srid)
mkLineString ls
  | U.length v >= 2 = Just $ LineString v
  | otherwise = Nothing
  where v = U.fromList ls

mkLinearRing :: VectorSpace v => [Point v srid] -> Maybe (LinearRing v srid)
mkLinearRing ls
  | U.length v >= 4, U.last v == U.head v = Just $ LinearRing v
  | otherwise = Nothing
  where v = U.fromList ls

mkPolygon :: [LinearRing v srid] -> Maybe (Polygon v srid)
mkPolygon (oRing:rings) = Just $ Polygon oRing $ V.fromList rings
mkPolygon _ = Nothing

mkTriangle :: VectorSpace v
  => Point v srid -> Point v srid -> Point v srid -> Maybe (Triangle v srid)
mkTriangle a b c | a/=b, b/=c, a/=c = Just $ Triangle a b c
                 | otherwise = Nothing

pointCoordinates :: VectorSpace v => Point v srid -> [Double]
pointCoordinates = views pVertex coords

lineStringCoordinates :: VectorSpace v => LineString v srid -> [[Double]]
lineStringCoordinates = vectorCoordinates . _lsPoints

linearRingCoordinates :: VectorSpace v => LinearRing v srid -> [[Double]]
linearRingCoordinates = vectorCoordinates . _lrPoints

polygonCoordinates :: VectorSpace v => Polygon v srid -> [[[Double]]]
polygonCoordinates = V.toList . V.map linearRingCoordinates . polygonRings

polygonRings :: Polygon v srid -> V.Vector (LinearRing v srid)
polygonRings (Polygon ir rs) = V.cons ir rs

triangleCoordinates :: VectorSpace v => Triangle v srid -> [[Double]]
triangleCoordinates (Triangle a b c) = map pointCoordinates [a, b, c, a]

vectorCoordinates :: VectorSpace v => U.Vector (Point v srid) -> [[Double]]
vectorCoordinates = V.toList . V.map pointCoordinates . V.convert

-- | A feature of 'GeometryType' t, vertex type 'v' and associated data 'd'
data FeatureT (g :: (* -> *) -> Nat -> *) v (srid::Nat) d = Feature {
    _fGeom :: g v srid
  , _fData :: d
  } deriving (Eq, Show, Functor)
makeLenses ''FeatureT

type Feature = FeatureT Geometry

newtype FeatureCollectionT (g :: (* -> *) -> Nat -> *) v (srid::Nat) d = FeatureCollection {
    _fcFeatures :: [FeatureT g v srid d]
} deriving (Eq, Show, Functor)
makeLenses ''FeatureCollectionT

type FeatureCollection = FeatureCollectionT Geometry

instance Monoid (FeatureCollectionT g v srid d) where
    mempty = FeatureCollection mempty
    (FeatureCollection as) `mappend` (FeatureCollection bs)
        = FeatureCollection $ as `mappend` bs

data Raster vs (t :: OffsetType) srid v a
  = Raster {
      rGeoReference :: !(GeoReference vs srid)
    , rData         :: !(v a)
    } deriving (Eq, Show)

indexRaster
  :: forall vs t srid v a. (GV.Vector v a, HasOffset vs t)
  => Raster vs t srid v a -> Pixel vs -> Maybe a
indexRaster raster px = fmap (GV.unsafeIndex arr . unOff) offset
  where
    offset :: Maybe (Offset t)
    offset = toOffset (grSize (rGeoReference raster)) px
    arr    = rData raster
{-# INLINE indexRaster #-}


convertRasterOffsetType
  :: forall vs t1 t2 srid v a. (GV.Vector v a, HasOffset vs t1, HasOffset vs t2)
  => Raster vs t1 srid v a -> Raster vs t2 srid v a
convertRasterOffsetType r = r {rData = GV.generate n go}
  where go i = let px        = unsafeFromOffset s (Offset i :: Offset t2)
                   Offset i' = unsafeToOffset s px :: Offset t1
               in rd `GV.unsafeIndex` i'
        s = grSize (rGeoReference r)
        rd = rData r
        n  = GV.length rd
