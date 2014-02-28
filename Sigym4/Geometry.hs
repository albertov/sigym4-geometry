{-# LANGUAGE StandaloneDeriving
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
  , GeometryType (..)
  , Feature (..)
  , IsVertex
  , Vertex
  , Pixel (..)
  , Size (..)
  , Extent (..)
  , SpatialReference (..)
  , GeoReference
  , mkGeoReference
  , vertexOffset
  , scalarSize
  , grScalarSize
  , grSize
  , forward
  , backward
  , module V2
  , module V3
) where

import Prelude hiding (product)
import Control.Applicative (Applicative, pure)
import Control.Lens
import Linear.Trace (Trace)
import Linear.Vector (Additive)
import Data.Foldable (Foldable)
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.Matrix ((!*), (*!), eye2, eye3, inv22, inv33)
import Linear.Epsilon (Epsilon)
import Data.Typeable
import Data.Foldable (product)

type Coord = Double
type Vertex v = v Coord
-- | A squared Matrix
type SqMatrix v = v (Vertex v)

-- | class for vertex types, used to simplify type signatures
class ( Num (Vertex v), Eq (Vertex v), Show (Vertex v), Fractional (Vertex v)
      , Num (SqMatrix v), Show (SqMatrix v), Eq (SqMatrix v)
      ,Applicative v, Additive v, Foldable v, Trace v)
  => IsVertex v
  where
    inv       :: SqMatrix v -> Maybe (SqMatrix v)
    eye       :: SqMatrix v
    linOffset :: Size v -> Pixel v -> Int

instance IsVertex V2 where
    inv = inv22
    eye = eye2
    linOffset s p = p'^._y * s'^._x
                  + p'^._x
      where p' = fmap truncate $ unPx p
            s' = fmap ceiling  $ unSize s

instance IsVertex V3 where
    inv = inv33
    eye = eye3
    linOffset s p = p'^._z * (s'^._x * s'^._y)
                  + p'^._y * s'^._x
                  + p'^._x
      where p' = fmap truncate $ unPx p
            s' = fmap ceiling  $ unSize s


-- | An enumeration of geometry types
data GeometryType = Point
    deriving (Show, Eq, Enum)

deriving instance Typeable 'Point

-- | A GADT used to represent different geometry types, each constructor returns
--   a geometry type indexed by 'GeometryType'
data Geometry (t :: GeometryType) v where
    MkPoint :: forall v. IsVertex v =>
      {pVertex :: !(Vertex v)} -> Geometry Point v
deriving instance Eq (Geometry t v)
deriving instance Show (Geometry t v)
deriving instance Typeable Geometry

-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v where
    Extent :: forall v. IsVertex v =>
      {eMin :: !(Vertex v), eMax :: !(Vertex v)} -> Extent v
deriving instance Eq (Extent v)
deriving instance Show (Extent v)
deriving instance Typeable Extent

-- | A pixel is a pseudo-newtype around a vertex
data Pixel v where
    Pixel :: forall v. IsVertex v =>
      {unPx :: !(Vertex v)} -> Pixel v

deriving instance Eq (Pixel v)
deriving instance Show (Pixel v)
deriving instance Typeable Pixel

-- | A shape is a pseudo-newtype around a vertex, it represents an array of
--   dimensions (v1,..,vn)
data Size v where
    Size :: forall v. IsVertex v =>
      {unSize :: !(Vertex v)} -> Size v

deriving instance Eq (Size v)
deriving instance Show (Size v)
deriving instance Typeable Size

scalarSize :: IsVertex v => Size v -> Int
scalarSize = product . fmap ceiling . unSize

-- integralSize = fmap ceiling . unSize

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
    GeoTransform :: forall v. IsVertex v =>
      { gtMatrix :: !(SqMatrix v)
      , gtOrigin :: !(Vertex v)
      } -> GeoTransform v
deriving instance Eq (GeoTransform v)
deriving instance Show (GeoTransform v)
deriving instance Typeable GeoTransform

-- Makes a standard 'GeoTransform' for north-up images with no rotation
-- northUpGeoTransform :: Extent V2 -> Pixel V2 -> Either String (GeoTransform V2)
northUpGeoTransform ::
  (IsVertex v, R2 v, Eq (v Bool)) =>
  Extent v -> Size v -> Either String (GeoTransform v)
northUpGeoTransform e s
  | not isValidBox   = Left "northUpGeoTransform: invalid extent"
  | not isValidSize = Left "northUpGeoTransform: invalid shape"
  | otherwise        = Right $ GeoTransform matrix origin
  where
    isValidBox  = fmap (> 0) (eMax e - eMin e)  == pure True
    isValidSize = fmap (> 0) s'                 == pure True
    origin      = (eMin e) & _y .~ ((eMax e)^._y)
    s'          = unSize s
    dPx         = (eMax e - eMin e)/s' & _y %~ negate
    matrix      = pure dPx * eye

forwardGT :: IsVertex v => GeoTransform v -> Vertex v -> Pixel v
forwardGT gt v = Pixel $ m !* (v-v0)
  where Just m = inv $ gtMatrix gt
        v0     = gtOrigin gt


backwardGT :: IsVertex v => GeoTransform v -> Pixel v -> Vertex v
backwardGT gt p = v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v where
  GeoReference :: forall v. IsVertex v =>
      { grTransform :: GeoTransform v
      , grSize      :: Size v
      , grSrs       :: SpatialReference
      } -> GeoReference v

deriving instance Eq (GeoReference v)
deriving instance Show (GeoReference v)
deriving instance Typeable GeoReference

grScalarSize :: IsVertex v => GeoReference v -> Int
grScalarSize = scalarSize . grSize


vertexOffset :: IsVertex v => GeoReference v -> Vertex v -> Int
vertexOffset gr =  linOffset (grSize gr) . forward gr
{-# SPECIALIZE INLINE vertexOffset :: GeoReference V2 -> Vertex V2 -> Int #-}

forward :: IsVertex v => GeoReference v -> Vertex v -> Pixel v
forward gr = forwardGT (grTransform gr)
{-# INLINE forward #-}

backward :: IsVertex v => GeoReference v -> Pixel v -> Vertex v
backward gr = backwardGT (grTransform gr)
{-# INLINE backward #-}


mkGeoReference ::
  (IsVertex v, R2 v, Eq (v Bool)) =>
  Extent v -> Size v -> SpatialReference -> Either String (GeoReference v)
mkGeoReference e s srs = fmap (\gt -> GeoReference gt s srs)
                              (northUpGeoTransform e s)
