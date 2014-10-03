{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module Arbitrary where

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.Vector (fromList)
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry hiding (fromList)

instance Arbitrary t => Arbitrary (V2 t) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (V3 t) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Vertex v) => Arbitrary (Point v srid) where
    arbitrary = fmap Point arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (LinearRing v srid) where
    arbitrary = do ps <- (++) <$> vector 3 <*> arbitrary
                   return . fromJust . mkLinearRing $ ps ++ [head ps]

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (LineString v srid) where
    arbitrary = fmap (fromJust . mkLineString) $ (++) <$> vector 2 <*> arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Polygon v srid) where
    arbitrary = Polygon <$> arbitrary <*> fmap fromList (resized arbitrary)

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Triangle v srid) where
    arbitrary = do
        mRet <- mkTriangle <$> arbitrary <*> arbitrary <*> arbitrary
        maybe arbitrary return mRet

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (PolyhedralSurface v srid) where
    arbitrary = PolyhedralSurface <$> fmap fromList (resized arbitrary)

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (TIN v srid) where
    arbitrary = TIN <$> fmap U.fromList (resized arbitrary)


instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Geometry v srid) where
    arbitrary = oneof (geometryCollection:geometries)
        where
            geometryCollection =
              GeoCollection . fromList <$> (resized $ listOf (oneof geometries))
            point = GeoPoint <$> arbitrary
            lineString = GeoLineString <$> arbitrary
            polygon = GeoPolygon <$> arbitrary
            multiPolygon = GeoMultiPolygon <$> fmap fromList (resized arbitrary)
            multiPoint = GeoMultiPoint <$> fmap fromList (resized arbitrary)
            multiLineString = GeoMultiLineString <$> fmap fromList (resized arbitrary)
            triangle = GeoTriangle <$> arbitrary
            tin = GeoTIN <$> arbitrary
            psurface = GeoPolyhedralSurface <$> arbitrary
            geometries = [ point, lineString, polygon
                         , multiPoint, multiLineString, multiPolygon
                         , triangle, psurface, tin
                         ]

resized :: Gen a -> Gen a
resized = resize 15
