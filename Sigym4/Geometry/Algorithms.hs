{-# LANGUAGE FunctionalDependencies
           , RankNTypes
           , FlexibleContexts
           , FlexibleInstances
           , CPP
           #-}
module Sigym4.Geometry.Algorithms (
    HasExtent(..)
  , HasDistance(..)
  , HasCentroid(..)
  , HasPredicates(..)
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif
import Sigym4.Geometry.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Semigroup as SG
import qualified Linear.Metric as M


class HasPredicates a b where
    contains :: a -> b -> Bool

instance VectorSpace v => HasPredicates (Extent v srid) (Point v srid) where
    Extent{eMin=l, eMax=h} `contains` (Point v)
      = (fmap (>= 0) (v - l) == pure True) && (fmap (>= 0) (h - v) == pure True)

instance VectorSpace v =>
  HasPredicates (Extent v srid) (MultiPoint v srid) where
    ext `contains` (MultiPoint ps) = V.all (contains ext) ps

instance VectorSpace v => HasPredicates (Point v srid) (Extent v srid) where
    (Point v) `contains` (Extent lo hi) = v==lo && v==hi


instance VectorSpace v => HasPredicates (Extent v srid) (LinearRing v srid)
  where
    ext `contains` (LinearRing ps) = U.all (contains ext) ps

instance VectorSpace v => HasPredicates (Extent v srid) (LineString v srid)
  where
    ext `contains` (LineString ps) = U.all (contains ext) ps

instance VectorSpace v =>
  HasPredicates (Extent v srid) (MultiLineString v srid) where
    ext `contains` (MultiLineString ps) = V.all (contains ext) ps

instance VectorSpace v => HasPredicates (Extent v srid) (Polygon v srid) where
    ext `contains` (Polygon oRing _) = ext `contains` oRing

instance VectorSpace v => HasPredicates (Extent v srid) (Triangle v srid) where
    ext `contains` (Triangle a b c) = ext `contains` a &&
                                      ext `contains` b &&
                                      ext `contains` c

instance VectorSpace v =>
  HasPredicates (Extent v srid) (MultiPolygon v srid) where
    ext `contains` (MultiPolygon ps) = V.all (contains ext) ps

instance VectorSpace v =>
  HasPredicates (Extent v srid) (PolyhedralSurface v srid) where
    ext `contains` (PolyhedralSurface ps) = V.all (contains ext) ps

instance VectorSpace v => HasPredicates (Extent v srid) (TIN v srid) where
    ext `contains` (TIN ts) = U.all (contains ext) ts

instance VectorSpace v => HasPredicates (Extent v srid) (Geometry v srid) where
    ext `contains` (GeoPoint g)             = ext `contains` g
    ext `contains` (GeoMultiPoint g)        = ext `contains` g
    ext `contains` (GeoLineString g)        = ext `contains` g
    ext `contains` (GeoMultiLineString g)   = ext `contains` g
    ext `contains` (GeoPolygon g)           = ext `contains` g
    ext `contains` (GeoMultiPolygon g)      = ext `contains` g
    ext `contains` (GeoTriangle g)          = ext `contains` g
    ext `contains` (GeoPolyhedralSurface g) = ext `contains` g
    ext `contains` (GeoTIN g)               = ext `contains` g
    ext `contains` (GeoCollection g)        = V.all (contains ext) g

instance VectorSpace v => HasPredicates (Extent v srid) (Feature v srid d) where
    ext `contains`  f = ext `contains`  _fGeom f

instance VectorSpace v =>
  HasPredicates (Extent v srid) (FeatureCollection v srid d) where
    ext `contains` fc = all (contains ext) $ _fcFeatures fc

class VectorSpace v => HasCentroid a v srid where
    centroid :: a -> Point v srid

instance VectorSpace v => HasCentroid (Point v srid) v srid where
    centroid = id

instance VectorSpace v => HasCentroid (Extent v srid) v srid where
    centroid e = Point $ eMin e + (eSize e / 2)


class HasDistance a b where
    distance :: a -> b -> Double

instance VectorSpace v => HasDistance (Point v srid) (Point v srid) where
    distance (Point a) (Point b) = M.distance a b

class VectorSpace v => HasExtent a v srid | a->v, a->srid where
    extent :: a -> Extent v srid

instance VectorSpace v => HasExtent (Point v srid) v srid where
    extent (Point v) = Extent v v

instance VectorSpace v => HasExtent (MultiPoint v srid) v srid where
    extent = extentFromVector . V.convert . _mpPoints


instance VectorSpace v => HasExtent (LinearRing v srid) v srid where
    extent = extentFromVector . V.convert . _lrPoints

instance VectorSpace v => HasExtent (LineString v srid) v srid where
    extent = extentFromVector . V.convert . _lsPoints

instance VectorSpace v => HasExtent (MultiLineString v srid) v srid where
    extent = extentFromVector . _mlLineStrings

instance VectorSpace v => HasExtent (Polygon v srid) v srid where
    extent = extent . _pOuterRing

instance VectorSpace v => HasExtent (Triangle v srid) v srid where
    extent (Triangle a b c) = a' SG.<> b' SG.<> c'
        where a' = extent a
              b' = extent b
              c' = extent c

instance VectorSpace v => HasExtent (MultiPolygon v srid) v srid where
    extent = extentFromVector . _mpPolygons

instance VectorSpace v => HasExtent (PolyhedralSurface v srid) v srid where
    extent = extentFromVector . _psPolygons

instance VectorSpace v => HasExtent (TIN v srid) v srid where
    extent = extentFromVector . V.convert . _tinTriangles

instance VectorSpace v => HasExtent (Geometry v srid) v srid where
    extent (GeoPoint g) = extent g
    extent (GeoMultiPoint g) = extent g
    extent (GeoLineString g) = extent g
    extent (GeoMultiLineString g) = extent g
    extent (GeoPolygon g) = extent g
    extent (GeoMultiPolygon g) = extent g
    extent (GeoTriangle g) = extent g
    extent (GeoPolyhedralSurface g) = extent g
    extent (GeoTIN g) = extent g
    extent (GeoCollection g) = extentFromVector g

extentFromVector :: (HasExtent a v srid, VectorSpace v) => V.Vector a -> Extent v srid
extentFromVector v = V.foldl' (SG.<>) (V.head es) (V.tail es)
    where es = V.map extent v

instance VectorSpace v => HasExtent (Feature v srid d) v srid where
    extent = extent . _fGeom

instance VectorSpace v => HasExtent (FeatureCollection v srid d) v srid where
    extent = extentFromVector . V.map _fGeom . V.fromList . _fcFeatures
