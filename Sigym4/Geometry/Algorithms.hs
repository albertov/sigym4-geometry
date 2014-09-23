{-# LANGUAGE MultiParamTypeClasses
          , RankNTypes
          , FlexibleContexts
          , FlexibleInstances
          #-}
module Sigym4.Geometry.Algorithms (
    HasExtent(..)
  , HasDistance(..)
) where

import Sigym4.Geometry.Types
import qualified Data.Vector as V
import qualified Data.Semigroup as SG
import qualified Linear.Metric as M

class HasDistance a b where
    distance :: a -> b -> Double

instance VectorSpace v => HasDistance (Point v) (Point v) where
    distance (Point a) (Point b) = M.distance a b

class VectorSpace v => HasExtent a v where
    extent :: a -> Extent v

instance VectorSpace v => HasExtent (Point v) v where
    extent (Point v) = Extent v v

instance VectorSpace v => HasExtent (LinearRing v) v where
    extent = extentFromVector . V.convert . _lrPoints

instance VectorSpace v => HasExtent (LineString v) v where
    extent = extentFromVector . V.convert . _lsPoints

instance VectorSpace v => HasExtent (Polygon v) v where
    extent = extent . _pOuterRing

instance VectorSpace v => HasExtent (Geometry v) v where
    extent (GeoPoint g) = extent g
    extent (GeoMultiPoint g) = extentFromVector g
    extent (GeoLineString g) = extent g
    extent (GeoMultiLineString g) = extentFromVector g
    extent (GeoPolygon g) = extent g
    extent (GeoMultiPolygon g) = extentFromVector g
    extent (GeoCollection g) = extentFromVector g

extentFromVector :: (HasExtent a v, VectorSpace v) => V.Vector a -> Extent v
extentFromVector v = V.foldl' (SG.<>) (V.head es) (V.tail es)
    where es = V.map extent v

instance VectorSpace v => HasExtent (Feature v d) v where
    extent = extent . _fGeom

instance VectorSpace v => HasExtent (FeatureCollection v d) v where
    extent = extentFromVector . V.map _fGeom . V.fromList . _fcFeatures
