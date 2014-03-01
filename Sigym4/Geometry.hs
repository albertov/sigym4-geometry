{-# LANGUAGE GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           , GADTs
           , TypeFamilies
           , DataKinds
           , DeriveDataTypeable
           #-}
module Sigym4.Geometry (
    Geometry (..)
  , Point
  , Feature (..)
  , IsVertex
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
  , module V2
  , module V3
) where

import Prelude hiding (product)
import Control.Applicative (Applicative, pure)
import Control.Lens
import Linear.Trace (Trace)
import Linear.Vector (Additive)
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.Matrix ((!*), (*!), eye2, eye3, inv22, inv33)
import Linear.Epsilon (Epsilon)
import Data.Typeable
import Data.Foldable (product)

-- | A squared Matrix
type SqMatrix v a = v (v a)

-- | class for vertex types, used to simplify type signatures
class ( Num a, Eq a, Show a, Epsilon a, Floating a
      , Num (v a), Eq (v a), Show (v a)
      , Num (SqMatrix v a), Show (SqMatrix v a), Eq (SqMatrix v a)
      ,Applicative v, Additive v, Foldable v, Trace v)
  => IsVertex v a
  where
    inv :: SqMatrix v a -> Maybe (SqMatrix v a)
    eye :: SqMatrix v a

newtype Offset (t :: OffsetType) = Offset {unOff :: Int}
  deriving (Eq, Show, Ord, Num)

data OffsetType = RowMajor | ColumnMajor

type RowMajor = 'RowMajor
type ColumnMajor = 'ColumnMajor

class HasOffset v (t :: OffsetType) where
    toOffset :: Size v -> Pixel v -> Maybe (Offset t)

instance (Num a, Eq a, Show a, Epsilon a, Floating a) => IsVertex V2 a where
    inv = inv22
    eye = eye2

instance HasOffset V2 RowMajor where
    toOffset s p
      | between (pure 0) s' p' = Just (Offset o)
      | otherwise              = Nothing
      where o  = p'^._y * s'^._x
               + p'^._x
            s' = unSize s
            p' = fmap floor $ unPx p
    {-# INLINE toOffset #-}

between :: (Applicative v, Ord a, Num (v a), Num a, Eq (v Bool))
  => v a -> v a -> v a -> Bool
between lo hi v = (fmap (>  0) (hi - v) == pure True) &&
                  (fmap (>= 0) (v - lo) == pure True)

instance (Num a, Eq a, Show a, Epsilon a, Floating a) => IsVertex V3 a where
    inv = inv33
    eye = eye3

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


-- | An enumeration of geometry types
data GeometryType = Point
    deriving (Show, Eq, Enum)

type Point = 'Point
deriving instance Typeable Point

-- | A GADT used to represent different geometry types, each constructor returns
--   a geometry type indexed by 'GeometryType'
data Geometry (t :: GeometryType) v where
    MkPoint :: forall v. IsVertex v Double =>
      {pVertex :: !(v Double)} -> Geometry Point v
deriving instance Eq (Geometry t v)
deriving instance Show (Geometry t v)
deriving instance Typeable Geometry

-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v where
    Extent :: forall v. IsVertex v Double =>
      {eMin :: !(v Double), eMax :: !(v Double)} -> Extent v
deriving instance Eq (Extent v)
deriving instance Show (Extent v)
deriving instance Typeable Extent

-- | A pixel is a newtype around a vertex
newtype Pixel v = Pixel {unPx :: v Double}


-- | A Size is a pseudo-newtype around a vector, it represents the dimensions
--  of an array of shape (v1,..,vn)
data Size v where
    Size :: forall v. (Eq (v Int), Show (v Int)) =>
      {unSize :: !(v Int)} -> Size v

deriving instance Eq (Size v)
deriving instance Show (Size v)
deriving instance Typeable Size

scalarSize :: IsVertex v Double => Size v -> Int
scalarSize = product . unSize

-- | A feature of 'GeometryType' t, vertex type 'v' and associated data 'd'
data Feature t v d = Feature {
    fGeom :: Geometry t v
  , fData :: d
  } deriving (Eq, Show, Typeable)

-- A Spatial reference system
data SpatialReference = SrsProj4 String
                      | SrsEPSG  Int
                      | SrsWKT   String
    deriving (Eq,Show,Typeable)

-- A GeoTransform defines how we translate from geographic 'Vertex'es to
-- 'Pixel' coordinates and back. gtMatrix *must* be inversible so smart
-- constructors are provided
data GeoTransform v  where
    GeoTransform :: forall v. IsVertex v Double =>
      { gtMatrix :: !(SqMatrix v Double)
      , gtOrigin :: !(v Double)
      } -> GeoTransform v
deriving instance Eq (GeoTransform v)
deriving instance Show (GeoTransform v)
deriving instance Typeable GeoTransform

-- Makes a standard 'GeoTransform' for north-up images with no rotation
-- northUpGeoTransform :: Extent V2 -> Pixel V2 -> Either String (GeoTransform V2)
northUpGeoTransform ::
  (IsVertex v Double, R2 v, Eq (v Bool), Fractional (v Double))
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

gtForward :: IsVertex v Double => GeoTransform v -> v Double -> Pixel v
gtForward gt v = Pixel $ m !* (v-v0)
  where m   = fromMaybe (error "gtForward. non-inversible matrix")
                        (inv $ gtMatrix gt)
        v0  = gtOrigin gt


gtBackward :: IsVertex v Double => GeoTransform v -> Pixel v -> v Double
gtBackward gt p = v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v where
  GeoReference :: forall v. IsVertex v Double =>
      { grTransform :: GeoTransform v
      , grSize      :: Size v
      , grSrs       :: SpatialReference
      } -> GeoReference v

deriving instance Eq (GeoReference v)
deriving instance Show (GeoReference v)
deriving instance Typeable GeoReference

grScalarSize :: IsVertex v Double => GeoReference v -> Int
grScalarSize = scalarSize . grSize


vertexOffset :: (HasOffset v t, IsVertex v Double)
  => GeoReference v -> v Double -> Maybe (Offset t)
vertexOffset gr =  toOffset (grSize gr) . grForward gr
{-# SPECIALIZE INLINE
      vertexOffset :: GeoReference V2 -> V2 Double -> Maybe (Offset RowMajor)
      #-}

grForward :: IsVertex v Double => GeoReference v -> v Double -> Pixel v
grForward gr = gtForward (grTransform gr)
{-# INLINE grForward #-}

grBackward :: IsVertex v Double => GeoReference v -> Pixel v -> v Double
grBackward gr = gtBackward (grTransform gr)
{-# INLINE grBackward #-}


mkGeoReference ::
  ( IsVertex v Double, R2 v
  , Eq (v Bool), Fractional (v Double)) =>
  Extent v -> Size v -> SpatialReference -> Either String (GeoReference v)
mkGeoReference e s srs = fmap (\gt -> GeoReference gt s srs)
                              (northUpGeoTransform e s)
