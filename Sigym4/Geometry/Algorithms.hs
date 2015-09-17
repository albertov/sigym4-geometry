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
    contains :: (VectorSpace v, KnownNat srid) => a v srid -> b v srid -> Bool

instance HasPredicates Extent Point where
    Extent{eMin=l, eMax=h} `contains` (Point v)
      = (fmap (>= 0) (v - l) == pure True) && (fmap (>= 0) (h - v) == pure True)
    {-# INLINABLE contains #-}

instance HasPredicates Extent MultiPoint where
    ext `contains` (MultiPoint ps) = V.all (contains ext) ps
    {-# INLINABLE contains #-}

instance HasPredicates Point Extent where
    (Point v) `contains` (Extent lo hi) = v==lo && v==hi
    {-# INLINABLE contains #-}


instance HasPredicates Extent LinearRing
  where
    ext `contains` (LinearRing ps) = U.all (contains ext) ps
    {-# INLINABLE contains #-}

instance HasPredicates Extent LineString
  where
    ext `contains` (LineString ps) = U.all (contains ext) ps
    {-# INLINABLE contains #-}

instance HasPredicates Extent MultiLineString where
    ext `contains` (MultiLineString ps) = V.all (contains ext) ps
    {-# INLINABLE contains #-}

instance HasPredicates Extent Polygon where
    ext `contains` (Polygon oRing _) = ext `contains` oRing
    {-# INLINABLE contains #-}

instance HasPredicates Extent Triangle where
    ext `contains` (Triangle a b c) = ext `contains` a &&
                                      ext `contains` b &&
                                      ext `contains` c
    {-# INLINABLE contains #-}

instance HasPredicates Extent MultiPolygon where
    ext `contains` (MultiPolygon ps) = V.all (contains ext) ps
    {-# INLINABLE contains #-}

instance HasPredicates Extent PolyhedralSurface where
    ext `contains` (PolyhedralSurface ps) = V.all (contains ext) ps
    {-# INLINABLE contains #-}

instance HasPredicates Extent TIN where
    ext `contains` (TIN ts) = U.all (contains ext) ts
    {-# INLINABLE contains #-}

instance HasPredicates Extent Geometry where
    ext `contains` (GeoPoint g)             = ext `contains` g
    ext `contains` (GeoMultiPoint g)        = ext `contains` g
    ext `contains` (GeoLineString g)        = ext `contains` g
    ext `contains` (GeoMultiLineString g)   = ext `contains` g
    ext `contains` (GeoPolygon g)           = ext `contains` g
    ext `contains` (GeoMultiPolygon g)      = ext `contains` g
    ext `contains` (GeoTriangle g)          = ext `contains` g
    ext `contains` (GeoPolyhedralSurface g) = ext `contains` g
    ext `contains` (GeoTIN g)               = ext `contains` g
    ext `contains` (GeoCollection g)        = ext `contains` g
    {-# INLINABLE contains #-}

instance HasPredicates Extent GeometryCollection where
    ext `contains` (GeometryCollection ps) = V.all (contains ext) ps
    {-# INLINABLE contains #-}

class HasCentroid a where
    centroid :: (VectorSpace v, KnownNat srid) => a v srid -> Point v srid

instance HasCentroid Point where
    centroid = id
    {-# INLINABLE centroid #-}

instance HasCentroid Extent where
    centroid e = Point $ eMin e + (eSize e / 2)
    {-# INLINABLE centroid #-}


class HasDistance a b where
    distance :: (VectorSpace v, KnownNat srid) => a v srid -> b v srid -> Double

instance HasDistance Point Point where
    distance (Point a) (Point b) = M.distance a b
    {-# INLINABLE distance #-}

class HasExtent a where
    extent :: (VectorSpace v, KnownNat srid) => a v srid -> Extent v srid

instance HasExtent Point where
    extent (Point v) = Extent v v
    {-# INLINABLE extent #-}

instance HasExtent MultiPoint where
    extent = extentFromVector . V.convert . _mpPoints
    {-# INLINABLE extent #-}


instance HasExtent LinearRing where
    extent = extentFromVector . V.convert . _lrPoints
    {-# INLINABLE extent #-}

instance HasExtent LineString where
    extent = extentFromVector . V.convert . _lsPoints
    {-# INLINABLE extent #-}

instance HasExtent MultiLineString where
    extent = extentFromVector . _mlLineStrings
    {-# INLINABLE extent #-}

instance HasExtent Polygon where
    extent = extent . _pOuterRing
    {-# INLINABLE extent #-}

instance HasExtent Triangle where
    extent (Triangle a b c) = a' SG.<> b' SG.<> c'
        where a' = extent a
              b' = extent b
              c' = extent c
    {-# INLINABLE extent #-}

instance HasExtent MultiPolygon where
    extent = extentFromVector . _mpPolygons
    {-# INLINABLE extent #-}

instance HasExtent PolyhedralSurface where
    extent = extentFromVector . _psPolygons
    {-# INLINABLE extent #-}

instance HasExtent TIN where
    extent = extentFromVector . V.convert . _tinTriangles
    {-# INLINABLE extent #-}

instance HasExtent Geometry where
    extent (GeoPoint g) = extent g
    extent (GeoMultiPoint g) = extent g
    extent (GeoLineString g) = extent g
    extent (GeoMultiLineString g) = extent g
    extent (GeoPolygon g) = extent g
    extent (GeoMultiPolygon g) = extent g
    extent (GeoTriangle g) = extent g
    extent (GeoPolyhedralSurface g) = extent g
    extent (GeoTIN g) = extent g
    extent (GeoCollection g) = extent g
    {-# INLINABLE extent #-}

instance HasExtent GeometryCollection where
    extent = extentFromVector . _gcGeometries
    {-# INLINABLE extent #-}

extentFromVector
  :: (HasExtent a, VectorSpace v, KnownNat srid)
  => V.Vector (a v srid) -> Extent v srid
extentFromVector v = V.foldl' (SG.<>) (V.head es) (V.tail es)
  where es = V.map extent v
{-# INLINE extentFromVector #-}
