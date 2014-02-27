{-# LANGUAGE StandaloneDeriving
           , FlexibleContexts
           , RankNTypes
           , GADTs
           , TypeFamilies
           , DataKinds
           , DeriveDataTypeable
           #-}
module Sigym4.Geometry (
    Geometry (..)
  , GeometryType (..)
  , Feature
  , SpatialReference (..)
  , GeoReference
  , mkGeoReference
  , pure
  , module V2
  , module V3
) where

import Control.Applicative (Applicative, pure)
import Linear.V2 as V2
import Linear.V3 as V3
import Data.Typeable

data GeometryType = Point
    deriving (Show, Eq)
deriving instance Typeable 'Point

data Geometry (t :: GeometryType) v where
    MkPoint :: forall v. (Eq (v Double), Show (v Double)) =>
      {pVertex :: !(v Double)} -> Geometry Point v

data Extent v where
    Extent :: forall v. (Eq (v Double), Show (v Double)) =>
      {eMin :: !(v Double), eMax :: !(v Double)} -> Extent v
deriving instance Eq (Extent v)
deriving instance Show (Extent v)
deriving instance Typeable Extent

newtype Pixel v = Pixel {unPx :: v} deriving (Eq, Show, Typeable)

deriving instance Eq (Geometry t v)
deriving instance Show (Geometry t v)
deriving instance Typeable Geometry

data Feature t v d = Feature {
    fGeom :: Geometry t v
  , fData :: d
  } deriving (Eq, Show, Typeable)

data SpatialReference = SrsProj4 String
                      | SrsEPSG  Int
                      | SrsWKT   String
    deriving (Eq,Show,Typeable)

data GeoReference v where
  GeoReference :: forall v. ( Eq (Extent v), Eq (v Int)
                            , Show (Extent v), Show (v Int)) =>
      { grExtent :: Extent v
      , grShape  :: Pixel (v Int)
      , grSrs    :: SpatialReference
      } -> GeoReference v
deriving instance Eq (GeoReference v)
deriving instance Show (GeoReference v)
deriving instance Typeable GeoReference

mkGeoReference
  :: ( Num (v Double), Functor v, Eq (v Bool), Applicative v, Show (v Int)
     , Eq (v Int))
  => Extent v -> Pixel (v Int) -> SpatialReference
  -> Either String (GeoReference v)
mkGeoReference e s srs
  | not isValidBox   = Left "mkGeoReference: invalid extent"
  | not isValidShape = Left "mkGeoReference: invalid shape"
  | otherwise        = Right $ GeoReference e s srs
  where
    isValidBox   =  fmap (> 0) (eMax e - eMin e)  == pure True
    isValidShape =  fmap (> 0) (unPx s)           == pure True
