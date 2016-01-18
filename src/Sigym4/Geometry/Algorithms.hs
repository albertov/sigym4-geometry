{-# LANGUAGE UndecidableInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , KindSignatures
           , DataKinds
           , TypeOperators
           , ScopedTypeVariables
           , InstanceSigs
           , CPP
           , BangPatterns
           , StandaloneDeriving
           #-}
module Sigym4.Geometry.Algorithms (
    HasExtent(..)
  , HasDistance(..)
  , HasCentroid(..)
  , HasContains(..)
  , HasIntersects(..)
  , HasHyperplanes (..)
  , Direction
  , HyperPlaneDirections
  , almostEqVertex
  , combinations
  , extentCorners
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif
import Control.Applicative (liftA2, liftA3)
import Control.Monad (replicateM)
import Control.Lens hiding (contains)
import qualified Data.Foldable as F
import Sigym4.Geometry.Types
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Semigroup as SG
import qualified Linear.Metric as M
import Linear.Matrix ((!*), identity, transpose)
import Data.List (tails)
import Data.Maybe (catMaybes)
import GHC.TypeLits

class HasContains a b where
    contains :: (VectorSpace v) => a v (crs::Symbol) -> b v (crs::Symbol) -> Bool

instance HasContains Extent Point where
    Extent{eMin=l, eMax=h} `contains` (Point v) = vBetween l h v
    {-# INLINE contains #-}

instance HasContains Extent Extent where
    Extent{eMin=l1, eMax=h1} `contains` Extent{eMin=l2, eMax=h2}
      = vBetweenC l1 h1 l2 && vBetweenC l1 h1 h2
    {-# INLINE contains #-}

instance HasContains Extent MultiPoint where
    ext `contains` (MultiPoint ps) = G.all (contains ext) ps
    {-# INLINE contains #-}


instance HasContains Extent LinearRing where
    ext `contains` (LinearRing ps) = G.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent LineString where
    ext `contains` (LineString ps) = G.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent MultiLineString where
    ext `contains` (MultiLineString ps) = G.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent Polygon where
    ext `contains` (Polygon oRing _) = ext `contains` oRing
    {-# INLINE contains #-}

instance HasContains Extent Triangle where
    ext `contains` (Triangle a b c) = ext `contains` a &&
                                      ext `contains` b &&
                                      ext `contains` c
    {-# INLINE contains #-}

instance HasContains Extent MultiPolygon where
    ext `contains` (MultiPolygon ps) = G.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent PolyhedralSurface where
    ext `contains` (PolyhedralSurface ps) = G.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent TIN where
    ext `contains` (TIN ts) = G.all (contains ext) ts
    {-# INLINE contains #-}

instance HasContains Extent Geometry where
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
    {-# INLINE contains #-}

instance HasContains Extent GeometryCollection where
    ext `contains` (GeometryCollection ps) = G.all (contains ext) ps
    {-# INLINE contains #-}



class HasIntersects a b where
  intersects
    :: HasHyperplanes v => a v (crs::Symbol) -> b v (crs::Symbol) -> Bool

instance HasIntersects Extent Point where
  intersects = contains
  {-# INLINE intersects #-}

instance HasIntersects Point Extent where
  intersects = flip intersects
  {-# INLINE intersects #-}

instance HasIntersects Extent Extent where
  Extent{eMin=l1, eMax=h1} `intersects` Extent{eMin=l2, eMax=h2}
    = liftBinBool (>) (eSize intersection) (pure 0)
    where intersection = Extent (liftA2 max l1 l2) (liftA2 min h1 h2)
  {-# INLINE intersects #-}


instance HasIntersects Extent LineString where
  intersects
    :: forall v crs. HasHyperplanes v
    => Extent v (crs::Symbol) -> LineString v (crs::Symbol) -> Bool
  ext@Extent{eMin=lo, eMax=hi} `intersects` LineString ps
    = G.any id
    $ G.zipWith segmentIntersects ps (G.tail ps)
    where
      planes :: [HyperPlaneDirections v]
      planes = map unsafeFromCoords $
                 combinations (dim (Proxy :: Proxy v) - 1)
                 (coords (identity :: SqMatrix v))
      corners = filter (not . almostEqVertex lo) (extentCorners ext)
      segmentIntersects pa@(Point a) pb@(Point b)
        | ext `contains` pa = True
        | ext `contains` pb = True
        | otherwise         = G.any inRange (U.fromList planeIntersections)
        where
          inRange v = vBetweenC lo hi v && not (any (almostEqVertex v) corners)
          planeIntersections = catMaybes
                                [ lineHyperplaneMaybeIntersection (b-a) a p o
                                | p<-planes, o<-[lo,hi]]
  {-# INLINE intersects #-}

extentCorners
  :: forall v crs. VectorSpace v
  => Extent v crs -> [Vertex v]
extentCorners (Extent lo hi)  = map mkCorner (replicateM d [False,True])
  where
    d = dim (Proxy :: Proxy v)
    mkCorner = liftA3 (\l h up -> if up then h else l) lo hi . unsafeFromCoords
{-# INLINE extentCorners #-}


class HasCentroid a where
    centroid :: (VectorSpace v) => a v (crs::Symbol) -> Point v (crs::Symbol)

instance HasCentroid Point where
    centroid = id
    {-# INLINE centroid #-}

instance HasCentroid Extent where
    centroid e = Point $ (eMin e + eMax e) / 2
    {-# INLINE centroid #-}


class HasDistance a b where
    distance :: (VectorSpace v) => a v (crs::Symbol) -> b v (crs::Symbol) -> Double

instance HasDistance Point Point where
    distance (Point a) (Point b) = M.distance a b
    {-# INLINE distance #-}

class HasExtent a where
    extent :: (VectorSpace v) => a v (crs::Symbol) -> Extent v (crs::Symbol)

instance HasExtent Point where
    extent (Point v) = Extent v v
    {-# INLINE extent #-}

instance HasExtent MultiPoint where
    extent = views points (extentFromVector . G.convert)
    {-# INLINE extent #-}


instance HasExtent LinearRing where
    extent = views points (extentFromVector . G.convert)
    {-# INLINE extent #-}

instance HasExtent LineString where
    extent = views points (extentFromVector . G.convert)
    {-# INLINE extent #-}

instance HasExtent MultiLineString where
    extent = views lineStrings extentFromVector
    {-# INLINE extent #-}

instance HasExtent Polygon where
    extent = views outerRing extent
    {-# INLINE extent #-}

instance HasExtent Triangle where
    extent (Triangle a b c) = a' SG.<> b' SG.<> c'
        where a' = extent a
              b' = extent b
              c' = extent c
    {-# INLINE extent #-}

instance HasExtent MultiPolygon where
    extent = views polygons extentFromVector
    {-# INLINE extent #-}

instance HasExtent PolyhedralSurface where
    extent = views polygons extentFromVector
    {-# INLINE extent #-}

instance HasExtent TIN where
    extent = views triangles (extentFromVector . G.convert)
    {-# INLINE extent #-}

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
    {-# INLINE extent #-}

instance HasExtent GeometryCollection where
    extent = views geometries extentFromVector
    {-# INLINE extent #-}

extentFromVector
  :: (HasExtent a, VectorSpace v)
  => V.Vector (a v (crs::Symbol)) -> Extent v (crs::Symbol)
extentFromVector v = G.foldl' (SG.<>) (G.head es) (G.tail es)
  where es = G.map extent v
{-# INLINE extentFromVector #-}

liftBinBool
  :: VectorSpace v
  => (Double -> Double -> Bool) -> Vertex v -> Vertex v -> Bool
liftBinBool f a =  F.all id . liftA2 f a
{-# INLINE liftBinBool #-}

vLt, vLte :: VectorSpace v => Vertex v -> Vertex v -> Bool
vLt  = liftBinBool (<)
vLte = liftBinBool (<=)
{-# INLINE vLt #-}
{-# INLINE vLte #-}

vBetween, vBetweenC :: VectorSpace v => Vertex v -> Vertex v -> Vertex v -> Bool
vBetween  l h p = l `vLte` p && p `vLt`  h
vBetweenC l h p = l `vLte` p && p `vLte` h
{-# INLINE vBetween #-}
{-# INLINE vBetweenC #-}

type Direction v = Vertex v

type HyperPlaneDirections v = VPlanes v (Direction v)

class ( VectorSpace v
      , VectorSpace (VPlanes v)
      , KnownNat (VsDim v - 1)
      , CmpNat (VsDim v - 1) (VsDim (VPlanes v)) ~ 'EQ
      ) => HasHyperplanes v where
  type VPlanes v :: * -> *

  lineHyperplaneMaybeIntersection
    :: Direction v -> Vertex v -> HyperPlaneDirections v -> Vertex v
    -> Maybe (Vertex v)

  lineHyperplaneIntersection
    :: Direction v -> Vertex v -> HyperPlaneDirections v -> Vertex v -> Vertex v

  lineHyperplaneMaybeIntersection lDirection lOrigin pVectors pOrigin
    | invertible a = Just (lOrigin + fmap (*lineDelta) lDirection)
    | otherwise    = Nothing
    where
      x           = inv a !* (lOrigin - pOrigin)
      a           = transpose (extendedMatrix lDirection pVectors)
      lineDelta   = negate (head (coords x))
  {-# INLINE lineHyperplaneMaybeIntersection #-}

  lineHyperplaneIntersection lDirection lOrigin pVectors pOrigin
    = lOrigin + fmap (*lineDelta) lDirection
    where
      x           = inv a !* (lOrigin - pOrigin)
      a           = transpose (extendedMatrix lDirection pVectors)
      lineDelta   = negate (head (coords x))
  {-# INLINE lineHyperplaneIntersection #-}

  extendedMatrix :: Vertex v -> HyperPlaneDirections v -> SqMatrix v
  extendedMatrix v dirs = unsafeFromCoords (v:coords dirs)
  {-# INLINE extendedMatrix #-}

instance HasHyperplanes V2 where
  type VPlanes V2 = V1

  {-
  lineHyperplaneIntersection lDir lOrigin (V1 pDir) pOrigin
    = pOrigin + fmap (*a) pDir
    where
      m = transpose (V3 lDir (negate pDir) (pOrigin-lOrigin))
      a = case (nearZero (m^._x._x), nearZero (m^._y._x)) of
            (True,  False) -> (m^._x._z / m^._x._y)
            (False, True)  -> (m^._y._z / m^._y._y)
            (False, False) ->
                  let m1 = m  & _x %~ (fmap (/   (m  ^._x._x)))
                              & _y %~ (fmap (/   (m  ^._y._x)))
                      m2 = m1 & _y %~ (subtract (m1^._x))
                  in (m2^._y._z / m2^._y._y)
            (True,  True)  -> error "lineDirection is null"
  {-# INLINE lineHyperplaneIntersection #-}
  -}

  extendedMatrix a (V1 b) = V2 a b
  {-# INLINE extendedMatrix #-}

instance HasHyperplanes V3 where
  type VPlanes V3 = V2
  extendedMatrix a (V2 b c) = V3 a b c
  {-# INLINE extendedMatrix #-}

instance HasHyperplanes V4 where
  type VPlanes V4 = V3
  extendedMatrix a (V3 b c d) = V4 a b c d
  {-# INLINE extendedMatrix #-}

instance (KnownNat n, KnownNat (n-1)) => HasHyperplanes (V n) where
  type VPlanes (V n) = V (n - 1)


almostEqVertex :: VectorSpace v => Vertex v -> Vertex v -> Bool
almostEqVertex a b = nearZero (a-b)
{-# INLINE almostEqVertex #-}

combinations :: Int -> [a] -> [[a]]
combinations = go
  where
    go !0 _  = [[]]
    go !n lst = do
      (x:xs) <- tails lst
      rest   <- go (n-1) xs
      return $ x : rest
{-# INLINE combinations #-}
