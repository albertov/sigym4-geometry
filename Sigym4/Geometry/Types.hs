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
           , BangPatterns
           #-}
module Sigym4.Geometry.Types (
    Geometry (..)
  , LineString (..)
  , LinearRing (..)
  , Vertex
  , Point (..)
  , Polygon (..)
  , Triangle (..)
  , TIN (..)
  , PolyhedralSurface (..)
  , pVertex
  , Feature (..)
  , FeatureCollection (..)
  , fData
  , fGeom
  , VectorSpace (..)
  , HasOffset
  , Pixel (..)
  , Size (..)
  , Offset (..)
  , RowMajor
  , ColumnMajor
  , Extent (..)
  , SpatialReference (..)
  , GeoTransform (..)
  , GeoReference
  , mkGeoReference
  , vertexOffset
  , grScalarSize
  , grSize
  , grTransform
  , grSrs
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

  , eSize

  , module V2
  , module V3
) where

import Prelude hiding (product)
import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Lens
import Data.Proxy (Proxy(..))
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import qualified Data.Semigroup as SG
import Data.Foldable (product)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.Matrix ((!*), (*!), eye2, eye3, inv22, inv33)
import Linear.Trace (Trace)
import Linear.Vector (Additive)
import Linear.Metric (Metric)

-- | A vertex
type Vertex v = v Double

-- | A square Matrix
type SqMatrix v = v (Vertex v)

-- | A vector space
class ( Num (Vertex v), Fractional (Vertex v)
      , Show (Vertex v), Eq (Vertex v), U.Unbox (Vertex v)
      , Show (v Int), Eq (v Int), Eq (v Bool) --XXX
      , Num (SqMatrix v), Show (SqMatrix v), Eq (SqMatrix v)
      , SG.Semigroup (Extent v)
      , Metric v, Applicative v, Additive v, Foldable v, Trace v)
  => VectorSpace v where
    inv :: SqMatrix v -> Maybe (SqMatrix v)
    eye :: SqMatrix v 
    dim :: Proxy v -> Int
    toList :: Vertex v -> [Double]
    fromList :: [Double] -> Maybe (Vertex v)

instance VectorSpace V2 where
    inv = inv22
    eye = eye2
    dim _ = 2
    toList (V2 u v) = [u, v]
    fromList [u, v] = Just $ V2 u v
    fromList _ = Nothing
    {-# INLINE dim #-}
    {-# INLINE fromList #-}
    {-# INLINE toList #-}

instance VectorSpace V3 where
    inv = inv33
    eye = eye3
    dim _ = 3
    toList (V3 u v z) = [u, v, z]
    fromList [u, v, z] = Just $ V3 u v z
    fromList _ = Nothing
    {-# INLINE dim #-}
    {-# INLINE fromList #-}
    {-# INLINE toList #-}


newtype Offset (t :: OffsetType) = Offset {unOff :: Int}
  deriving (Eq, Show, Ord, Num)

data OffsetType = RowMajor | ColumnMajor

type RowMajor = 'RowMajor
type ColumnMajor = 'ColumnMajor

class HasOffset v (t :: OffsetType) where
    toOffset :: Size v -> Pixel v -> Maybe (Offset t)

instance HasOffset V2 RowMajor where
    toOffset s p
      | between (pure 0) s' p' = Just (Offset o)
      | otherwise              = Nothing
      where o  = p'^._y * s'^._x
               + p'^._x
            s' = unSize s
            p' = fmap floor $ unPx p
    {-# INLINE toOffset #-}

instance HasOffset V2 ColumnMajor where
    toOffset s p
      | between (pure 0) s' p' = Just (Offset o)
      | otherwise              = Nothing
      where o  = p'^._x * s'^._y
               + p'^._y
            s' = unSize s
            p' = fmap floor $ unPx p
    {-# INLINE toOffset #-}

between :: (Ord a, Num a, Num (v a), VectorSpace v) => v a -> v a -> v a -> Bool
between lo hi v = (fmap (>  0) (hi - v) == pure True) &&
                  (fmap (>= 0) (v - lo) == pure True)

instance HasOffset V3 RowMajor where
    toOffset s p
      | between (pure 0) s' p' = Just (Offset o)
      | otherwise              = Nothing
      where o  = p'^._z * (s'^._x * s'^._y)
               + p'^._y * s'^._x
               + p'^._x
            s' = unSize s
            p' = fmap floor $ unPx p
    {-# INLINE toOffset #-}

instance HasOffset V3 ColumnMajor where
    toOffset s p
      | between (pure 0) s' p' = Just (Offset o)
      | otherwise              = Nothing
      where o  = p'^._x * (s'^._z * s'^._y)
               + p'^._y * s'^._z
               + p'^._z
            s' = unSize s
            p' = fmap floor $ unPx p
    {-# INLINE toOffset #-}



-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v = Extent {eMin :: !(Vertex v), eMax :: !(Vertex v)}
deriving instance VectorSpace v => Eq (Extent v)
deriving instance VectorSpace v => Show (Extent v)

eSize :: VectorSpace v => Extent v -> v Double
eSize e = eMax e - eMin e

instance VectorSpace v => SG.Semigroup (Extent v) where
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


-- A Spatial reference system
data SpatialReference = SrsProj4 String
                      | SrsEPSG  Int
                      | SrsWKT   String
    deriving (Eq,Show)

-- A GeoTransform defines how we translate from geographic 'Vertex'es to
-- 'Pixel' coordinates and back. gtMatrix *must* be inversible so smart
-- constructors are provided
data GeoTransform v  = GeoTransform 
      { gtMatrix :: !(SqMatrix v)
      , gtOrigin :: !(Vertex v)
      }
deriving instance VectorSpace v => Eq (GeoTransform v)
deriving instance VectorSpace v => Show (GeoTransform v)

-- Makes a standard 'GeoTransform' for north-up images with no rotation
-- northUpGeoTransform :: Extent V2 -> Pixel V2 -> Either String (GeoTransform V2)
northUpGeoTransform ::
  (VectorSpace v, R2 v, Eq (v Bool), Fractional (Vertex v))
  => Extent v -> Size v -> Either String (GeoTransform v)
northUpGeoTransform e s
  | not isValidBox   = Left "northUpGeoTransform: invalid extent"
  | not isValidSize  = Left "northUpGeoTransform: invalid size"
  | otherwise        = Right $ GeoTransform matrix origin
  where
    isValidBox  = fmap (> 0) (eMax e - eMin e)  == pure True
    isValidSize = fmap (> 0) s'                 == pure True
    origin      = (eMin e) & _y .~ ((eMax e)^._y)
    s'          = fmap fromIntegral $ unSize s
    dPx         = (eMax e - eMin e)/s' & _y %~ negate
    matrix      = pure dPx * eye

gtForward :: VectorSpace v => GeoTransform v -> Vertex v -> Pixel v
gtForward gt v = Pixel $ m !* (v-v0)
  where m   = fromMaybe (error "gtForward. non-inversible matrix")
                        (inv $ gtMatrix gt)
        v0  = gtOrigin gt


gtBackward :: VectorSpace v => GeoTransform v -> Pixel v -> Vertex v
gtBackward gt p = v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v = GeoReference 
      { grTransform :: GeoTransform v
      , grSize      :: Size v
      , grSrs       :: SpatialReference
      }
deriving instance VectorSpace v => Eq (GeoReference v)
deriving instance VectorSpace v => Show (GeoReference v)


grScalarSize :: VectorSpace v => GeoReference v -> Int
grScalarSize = scalarSize . grSize


vertexOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v -> Vertex v -> Maybe (Offset t)
vertexOffset gr =  toOffset (grSize gr) . grForward gr
{-# SPECIALIZE INLINE
      vertexOffset :: GeoReference V2 -> V2 Double -> Maybe (Offset RowMajor)
      #-}

grForward :: VectorSpace v => GeoReference v -> Vertex v -> Pixel v
grForward gr = gtForward (grTransform gr)
{-# INLINE grForward #-}

grBackward :: VectorSpace v => GeoReference v -> Pixel v -> Vertex v
grBackward gr = gtBackward (grTransform gr)
{-# INLINE grBackward #-}


mkGeoReference ::
  ( VectorSpace v, R2 v
  , Eq (v Bool), Fractional (Vertex v)) =>
  Extent v -> Size v -> SpatialReference -> Either String (GeoReference v)
mkGeoReference e s srs = fmap (\gt -> GeoReference gt s srs)
                              (northUpGeoTransform e s)

newtype Point v = Point {_pVertex:: Vertex v}
deriving instance VectorSpace v => Show (Point v)
deriving instance VectorSpace v => Eq (Point v)

derivingUnbox "Point"
    [t| VectorSpace v => Point v -> Vertex v |]
    [| \(Point v) -> v |]
    [| \v -> Point v|]

newtype LinearRing v = LinearRing {_lrPoints :: U.Vector (Point v)}
    deriving (Eq, Show)

newtype LineString v = LineString {_lsPoints :: U.Vector (Point v)}
    deriving (Eq, Show)

data Triangle v = Triangle !(Point v) !(Point v) !(Point v)
    deriving (Eq, Show)

derivingUnbox "Triangle"
    [t| VectorSpace v => Triangle v -> (Point v, Point v, Point v) |]
    [| \(Triangle a b c) -> (a, b, c) |]
    [| \(a, b, c) -> Triangle a b c|]

data Polygon v = Polygon {
    _pOuterRing :: LinearRing v
  , _pRings     :: V.Vector (LinearRing v)
} deriving (Eq, Show)

newtype PolyhedralSurface v = PolyhedralSurface {
    _psPolygons :: V.Vector (Polygon v)
} deriving (Eq, Show)

newtype TIN v = TIN {
    _tinTriangles :: U.Vector (Triangle v)
} deriving (Eq, Show)

data Geometry v
    = GeoPoint (Point v)
    | GeoMultiPoint (V.Vector (Point v))
    | GeoLineString (LineString v)
    | GeoMultiLineString (V.Vector (LineString v))
    | GeoPolygon (Polygon v)
    | GeoMultiPolygon (V.Vector (Polygon v))
    | GeoTriangle (Triangle v)
    | GeoPolyhedralSurface (PolyhedralSurface v)
    | GeoTIN (TIN v)
    | GeoCollection (V.Vector (Geometry v))
    deriving (Eq, Show)


mkLineString :: VectorSpace v => [Point v] -> Maybe (LineString v)
mkLineString ls
  | U.length v >= 2 = Just $ LineString v
  | otherwise = Nothing
  where v = U.fromList ls

mkLinearRing :: VectorSpace v => [Point v] -> Maybe (LinearRing v)
mkLinearRing ls
  | U.length v >= 4, U.last v == U.head v = Just $ LinearRing v
  | otherwise = Nothing
  where v = U.fromList ls

mkPolygon :: [LinearRing v] -> Maybe (Polygon v)
mkPolygon (oRing:rings) = Just $ Polygon oRing $ V.fromList rings
mkPolygon _ = Nothing

mkTriangle :: VectorSpace v
  => Point v -> Point v -> Point v -> Maybe (Triangle v)
mkTriangle a b c | a/=b, b/=c, a/=c = Just $ Triangle a b c
                 | otherwise = Nothing

pVertex :: VectorSpace v => Lens' (Point v) (Vertex v)
pVertex = lens _pVertex (\point v -> point { _pVertex = v })
{-# INLINE pVertex #-}

pointCoordinates :: VectorSpace v => Point v -> [Double]
pointCoordinates = toList . _pVertex

lineStringCoordinates :: VectorSpace v => LineString v -> [[Double]]
lineStringCoordinates = vectorCoordinates . _lsPoints

linearRingCoordinates :: VectorSpace v => LinearRing v -> [[Double]]
linearRingCoordinates = vectorCoordinates . _lrPoints

polygonCoordinates :: VectorSpace v => Polygon v -> [[[Double]]]
polygonCoordinates = V.toList . V.map linearRingCoordinates . polygonRings

polygonRings :: Polygon v -> V.Vector (LinearRing v)
polygonRings (Polygon ir rs) = V.cons ir rs

triangleCoordinates :: VectorSpace v => Triangle v -> [[Double]]
triangleCoordinates (Triangle a b c) = map pointCoordinates [a, b, c, a]

vectorCoordinates :: VectorSpace v => U.Vector (Point v) -> [[Double]]
vectorCoordinates = V.toList . V.map pointCoordinates . V.convert

-- | A feature of 'GeometryType' t, vertex type 'v' and associated data 'd'
data Feature v d = Feature {
    _fGeom :: Geometry v
  , _fData :: d
  } deriving (Eq, Show)
makeLenses ''Feature

newtype FeatureCollection v d = FeatureCollection {
    _fcFeatures :: [Feature v d]
} deriving (Show)

instance Monoid (FeatureCollection v d) where
    mempty = FeatureCollection mempty
    (FeatureCollection as) `mappend` (FeatureCollection bs)
        = FeatureCollection $ as `mappend` bs

instance Functor (Feature v) where
   fmap f (Feature g d) = Feature g (f d)
