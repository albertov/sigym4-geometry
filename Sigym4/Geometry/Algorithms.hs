{-# LANGUAGE MultiParamTypeClasses
          , RankNTypes
          , FlexibleContexts
          , FlexibleInstances
          #-}
module Sigym4.Geometry.Algorithms (
    HasExtent(..)
  , HasDistance(..)
  , HasCentroid(..)
  , HasPredicates(..)
) where

import Control.Applicative (pure)
import Sigym4.Geometry.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Semigroup as SG
import qualified Linear.Metric as M


class HasPredicates a b where
    contains :: a -> b -> Bool

instance VectorSpace v => HasPredicates (Extent v) (Point v) where
    Extent{eMin=l, eMax=h} `contains` (Point v)
      = (fmap (>= 0) (v - l) == pure True) && (fmap (>= 0) (h - v) == pure True)

instance VectorSpace v => HasPredicates (Point v) (Extent v) where
    (Point v) `contains` (Extent lo hi) = v==lo && v==hi


instance VectorSpace v => HasPredicates (Extent v) (LinearRing v) where
    ext `contains` (LinearRing ps) = U.all (contains ext) ps

instance VectorSpace v => HasPredicates (Extent v) (LineString v) where
    ext `contains` (LineString ps) = U.all (contains ext) ps

instance VectorSpace v => HasPredicates (Extent v) (Polygon v) where
    ext `contains` (Polygon oRing _) = ext `contains` oRing

instance VectorSpace v => HasPredicates (Extent v) (Triangle v) where
    ext `contains` (Triangle a b c) = ext `contains` a &&
                                      ext `contains` b &&
                                      ext `contains` c

instance VectorSpace v => HasPredicates (Extent v) (Geometry v) where
    ext `contains` (GeoPoint g) = ext `contains` g
    ext `contains` (GeoMultiPoint g) = V.all (contains ext) g
    ext `contains` (GeoLineString g) = ext `contains` g
    ext `contains` (GeoMultiLineString g) = V.all (contains ext) g
    ext `contains` (GeoPolygon g) = ext `contains` g
    ext `contains` (GeoMultiPolygon g) = V.all (contains ext) g
    ext `contains` (GeoTriangle g) = ext `contains` g
    ext `contains` (GeoCollection g) = V.all (contains ext) g

instance VectorSpace v => HasPredicates (Extent v) (Feature v d) where
    ext `contains`  f = ext `contains`  _fGeom f

instance VectorSpace v => HasPredicates (Extent v) (FeatureCollection v d) where
    ext `contains` fc = all (contains ext) $ _fcFeatures fc

class VectorSpace v => HasCentroid a v where
    centroid :: a -> Point v

instance VectorSpace v => HasCentroid (Point v) v where
    centroid = id

instance VectorSpace v => HasCentroid (Extent v) v where
    centroid e = Point $ eMin e + (eSize e / 2)


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

instance VectorSpace v => HasExtent (Triangle v) v where
    extent (Triangle a b c) = a' SG.<> b' SG.<> c'
        where a' = extent a
              b' = extent b
              c' = extent c

instance VectorSpace v => HasExtent (Geometry v) v where
    extent (GeoPoint g) = extent g
    extent (GeoMultiPoint g) = extentFromVector g
    extent (GeoLineString g) = extent g
    extent (GeoMultiLineString g) = extentFromVector g
    extent (GeoPolygon g) = extent g
    extent (GeoMultiPolygon g) = extentFromVector g
    extent (GeoTriangle g) = extent g
    extent (GeoCollection g) = extentFromVector g

extentFromVector :: (HasExtent a v, VectorSpace v) => V.Vector a -> Extent v
extentFromVector v = V.foldl' (SG.<>) (V.head es) (V.tail es)
    where es = V.map extent v

instance VectorSpace v => HasExtent (Feature v d) v where
    extent = extent . _fGeom

instance VectorSpace v => HasExtent (FeatureCollection v d) v where
    extent = extentFromVector . V.map _fGeom . V.fromList . _fcFeatures
