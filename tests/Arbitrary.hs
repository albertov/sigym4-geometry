{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , ScopedTypeVariables
           , UndecidableInstances
           #-}
module Arbitrary where

import Test.QuickCheck
import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Data.Vector (fromList)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry

instance {-# INCOHERENT #-} forall v. VectorSpace v
  => Arbitrary (Vertex v) where
  arbitrary = unsafeFromCoords <$> replicateM n arbitrary
    where n = dim (Proxy :: Proxy v)

positiveV :: forall v. VectorSpace v => Gen (Vertex v)
positiveV = unsafeFromCoords <$> replicateM n (choose (1, 1000))
  where n = dim (Proxy :: Proxy v)

instance VectorSpace v => Arbitrary (Size v) where
  arbitrary = (Size . fmap round) <$> positiveV

instance Arbitrary (Offset t) where
  arbitrary = Offset . getNonNegative <$> arbitrary

instance VectorSpace v => Arbitrary (Extent v) where
  arbitrary = do
    lr <- arbitrary
    d  <- positiveV
    return $ Extent lr (lr+d)

instance Arbitrary (Vertex v) => Arbitrary (Pixel v) where
  arbitrary = Pixel <$> arbitrary

instance Arbitrary (Vertex v) => Arbitrary (Point v) where
    arbitrary = fmap Point arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (MultiPoint v) where
    arbitrary = MultiPoint <$> fmap U.fromList (resized arbitrary)

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (LinearRing v) where
    arbitrary = do ps <- (++) <$> vector 3 <*> arbitrary
                   return . fromJust . mkLinearRing $ ps ++ [head ps]

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (LineString v) where
    arbitrary = fmap (fromJust . mkLineString) $ (++) <$> vector 2 <*> arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (MultiLineString v) where
    arbitrary = MultiLineString <$> fmap fromList (resized arbitrary)


instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Polygon v) where
    arbitrary = Polygon <$> arbitrary <*> fmap fromList (resized arbitrary)

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Triangle v) where
    arbitrary = do
        mRet <- mkTriangle <$> arbitrary <*> arbitrary <*> arbitrary
        maybe arbitrary return mRet

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (MultiPolygon v) where
    arbitrary = MultiPolygon <$> fmap fromList (resized arbitrary)

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (PolyhedralSurface v) where
    arbitrary = PolyhedralSurface <$> fmap fromList (resized arbitrary)

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (TIN v) where
    arbitrary = TIN <$> fmap U.fromList (resized arbitrary)


instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Geometry v) where
    arbitrary = oneof (geometryCollection:geometries)
        where
            geometryCollection = GeoCollection . GeometryCollection . fromList
                             <$> (resized $ listOf (oneof geometries))
            point = GeoPoint <$> arbitrary
            lineString = GeoLineString <$> arbitrary
            polygon = GeoPolygon <$> arbitrary
            multiPolygon = GeoMultiPolygon <$> arbitrary
            multiPoint = GeoMultiPoint <$> arbitrary
            multiLineString = GeoMultiLineString <$> arbitrary
            triangle = GeoTriangle <$> arbitrary
            tin = GeoTIN <$> arbitrary
            psurface = GeoPolyhedralSurface <$> arbitrary
            geometries = [ point, lineString, polygon
                         , multiPoint, multiLineString, multiPolygon
                         , triangle, psurface, tin
                         ]


resized :: Gen a -> Gen a
resized = resize 15
