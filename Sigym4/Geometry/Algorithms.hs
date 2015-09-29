{-# LANGUAGE FunctionalDependencies
           , RankNTypes
           , FlexibleContexts
           , FlexibleInstances
           , KindSignatures
           , DataKinds
           , TypeOperators
           , ScopedTypeVariables
           , InstanceSigs
           , CPP
           , BangPatterns
           #-}
module Sigym4.Geometry.Algorithms (
    HasExtent(..)
  , HasDistance(..)
  , HasCentroid(..)
  , HasContains(..)
  , HasIntersects(..)
  , Direction
  , lineHyperplaneIntersection
  , lineHyperplaneMaybeIntersection
  , almostEqVertex
  , combinations
  , extentCorners
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif
import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import qualified Data.Foldable as F
import Sigym4.Geometry.Types
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Semigroup as SG
import qualified Linear.Metric as M
import Linear.Matrix ((!*), identity)
import Data.List (tails)
import Data.Maybe (catMaybes)
import GHC.TypeLits

class HasContains a b where
    contains :: (VectorSpace v) => a v (srid::Nat) -> b v (srid::Nat) -> Bool

instance HasContains Extent Point where
    Extent{eMin=l, eMax=h} `contains` (Point v) = vBetween l h v
    {-# INLINE contains #-}

instance HasContains Extent Extent where
    Extent{eMin=l1, eMax=h1} `contains` Extent{eMin=l2, eMax=h2}
      = vBetweenC l1 h1 l2 && vBetweenC l1 h1 h2
    {-# INLINE contains #-}

instance HasContains Extent MultiPoint where
    ext `contains` (MultiPoint ps) = V.all (contains ext) ps
    {-# INLINE contains #-}


instance HasContains Extent LinearRing where
    ext `contains` (LinearRing ps) = U.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent LineString where
    ext `contains` (LineString ps) = U.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent MultiLineString where
    ext `contains` (MultiLineString ps) = V.all (contains ext) ps
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
    ext `contains` (MultiPolygon ps) = V.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent PolyhedralSurface where
    ext `contains` (PolyhedralSurface ps) = V.all (contains ext) ps
    {-# INLINE contains #-}

instance HasContains Extent TIN where
    ext `contains` (TIN ts) = U.all (contains ext) ts
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
    ext `contains` (GeometryCollection ps) = V.all (contains ext) ps
    {-# INLINE contains #-}



class HasIntersects a b where
  intersects
    :: (VectorSpace v, KnownNat (VsDim v -1))
    => a v (srid::Nat) -> b v (srid::Nat) -> Bool

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
    :: forall v srid. (VectorSpace v, KnownNat (VsDim v -1))
    => Extent v (srid::Nat) -> LineString v (srid::Nat) -> Bool
  ext@Extent{eMin=lo, eMax=hi} `intersects` LineString ps
    = U.any id
    $ U.zipWith segmentIntersects ps (U.tail ps)
    where
      planes :: [V (VsDim v - 1) (Direction v)]
      planes = map (V . V.fromList) $
               combinations (dim (Proxy :: Proxy v) - 1)
               (V.toList (toVector (toVectorN (identity :: SqMatrix v))))
      corners = filter (not . almostEqVertex lo) (extentCorners ext)
      segmentIntersects pa@(Point a) pb@(Point b)
        | ext `contains` pa = True
        | ext `contains` pb = True
        | otherwise         = U.any inRange (U.fromList planeIntersections)
        where
          inRange v = vBetweenC lo hi v && not (any (almostEqVertex v) corners)
          planeIntersections = catMaybes 
                                [ lineHyperplaneMaybeIntersection (b-a) a p o
                                | p<-planes, o<-[lo,hi]]
  {-# INLINE intersects #-}

extentCorners 
  :: forall v srid. VectorSpace v
  => Extent v srid -> [Vertex v]
extentCorners (Extent lo hi)  = map mkCorner (replicateM d [False,True])
  where
    d = dim (Proxy :: Proxy v)
    vlo = toVector (toVectorN lo)
    vhi = toVector (toVectorN hi)
    mkCorner ls = fromVectorN $ V $
                   V.zipWith3 (\up l h -> if up then h else l)
                              (V.fromList ls) vlo vhi


class HasCentroid a where
    centroid :: (VectorSpace v) => a v (srid::Nat) -> Point v (srid::Nat)

instance HasCentroid Point where
    centroid = id
    {-# INLINE centroid #-}

instance HasCentroid Extent where
    centroid e = Point $ (eMin e + eMax e) / 2
    {-# INLINE centroid #-}


class HasDistance a b where
    distance :: (VectorSpace v) => a v (srid::Nat) -> b v (srid::Nat) -> Double

instance HasDistance Point Point where
    distance (Point a) (Point b) = M.distance a b
    {-# INLINE distance #-}

class HasExtent a where
    extent :: (VectorSpace v) => a v (srid::Nat) -> Extent v (srid::Nat)

instance HasExtent Point where
    extent (Point v) = Extent v v
    {-# INLINE extent #-}

instance HasExtent MultiPoint where
    extent = extentFromVector . V.convert . _mpPoints
    {-# INLINE extent #-}


instance HasExtent LinearRing where
    extent = extentFromVector . V.convert . _lrPoints
    {-# INLINE extent #-}

instance HasExtent LineString where
    extent = extentFromVector . V.convert . _lsPoints
    {-# INLINE extent #-}

instance HasExtent MultiLineString where
    extent = extentFromVector . _mlLineStrings
    {-# INLINE extent #-}

instance HasExtent Polygon where
    extent = extent . _pOuterRing
    {-# INLINE extent #-}

instance HasExtent Triangle where
    extent (Triangle a b c) = a' SG.<> b' SG.<> c'
        where a' = extent a
              b' = extent b
              c' = extent c
    {-# INLINE extent #-}

instance HasExtent MultiPolygon where
    extent = extentFromVector . _mpPolygons
    {-# INLINE extent #-}

instance HasExtent PolyhedralSurface where
    extent = extentFromVector . _psPolygons
    {-# INLINE extent #-}

instance HasExtent TIN where
    extent = extentFromVector . V.convert . _tinTriangles
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
    extent = extentFromVector . _gcGeometries
    {-# INLINE extent #-}

extentFromVector
  :: (HasExtent a, VectorSpace v)
  => V.Vector (a v (srid::Nat)) -> Extent v (srid::Nat)
extentFromVector v = V.foldl' (SG.<>) (V.head es) (V.tail es)
  where es = V.map extent v
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

lineHyperplaneMaybeIntersection
  :: forall v. (VectorSpace v, KnownNat (VsDim v - 1))
  => Direction v -> Vertex v -> V (VsDim v - 1) (Direction v) -> Vertex v
  -> Maybe (Vertex v)
lineHyperplaneMaybeIntersection lineDirection lineOrigin planeDirections planeOrigin
  | valid     = Just v
  | otherwise = Nothing
  where
    (valid,v) = lineHyperplaneIntersection'
                  lineDirection lineOrigin planeDirections planeOrigin
{-# INLINE lineHyperplaneMaybeIntersection #-}

lineHyperplaneIntersection
  :: forall v. (VectorSpace v, KnownNat (VsDim v - 1))
  => Direction v -> Vertex v -> V (VsDim v - 1) (Direction v) -> Vertex v
  -> Vertex v
lineHyperplaneIntersection lineDirection lineOrigin planeDirections
  = snd . lineHyperplaneIntersection' lineDirection lineOrigin planeDirections
{-# INLINE lineHyperplaneIntersection #-}


lineHyperplaneIntersection'
  :: forall v. (VectorSpace v, KnownNat (VsDim v - 1))
  => Direction v -> Vertex v -> V (VsDim v - 1) (Direction v) -> Vertex v
  -> (Bool, Vertex v)
lineHyperplaneIntersection' lineDirection lineOrigin planeDirections planeOrigin
  = (invertible a, lineOrigin + fmap (*lineDelta) lineDirection)
  where
    x           = inv a !* (lineOrigin - planeOrigin)
    lineDelta   = negate (V.head (toVector (toVectorN x)))
    planeDirs   = toVector (toVectorN planeDirections)
    lineDir     = toVector (toVectorN lineDirection)
    d           = dim (Proxy :: Proxy v)
    a           = fromVectorN (V (V.generate d genRow))
    genRow i    = fromVectorN (V (V.generate d (genCell i)))
    genCell i j = v `V.unsafeIndex` i
      where
        v | j==0      = lineDir
          | otherwise = toVector (toVectorN (planeDirs `V.unsafeIndex` (j-1)))
{-# INLINE lineHyperplaneIntersection' #-}

almostEqVertex :: VectorSpace v => Vertex v -> Vertex v -> Bool
almostEqVertex a b = nearZero (a-b)
{-# INLINE almostEqVertex #-}

combinations :: Int -> [a] -> [[a]]
combinations = go
  where
    go !n []  = [[]]
    go !n lst = do
      (x:xs) <- tails lst
      rest   <- go (n-1) xs
      return $ x : rest
{-# INLINE combinations #-}
