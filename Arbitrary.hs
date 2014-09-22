{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module Arbitrary where

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.Vector (fromList)

import Sigym4.Geometry hiding (fromList)

instance Arbitrary t => Arbitrary (V2 t) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (V3 t) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Vertex v) => Arbitrary (Point v) where
    arbitrary = fmap Point arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (LinearRing v) where
    arbitrary = do ps <- (++) <$> vector 3 <*> arbitrary
                   return . fromJust . mkLinearRing $ ps ++ [head ps]

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (LineString v) where
    arbitrary = fmap (fromJust . mkLineString) $ (++) <$> vector 2 <*> arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Polygon v) where
    arbitrary = Polygon <$> arbitrary <*> fmap fromList arbitrary

instance (VectorSpace v, Arbitrary (Vertex v))
  => Arbitrary (Geometry v) where
    arbitrary = oneof (geometryCollection:geometries)
        where
            point = GeoPoint <$> arbitrary
            multiPoint = GeoMultiPoint <$> fmap fromList arbitrary
            lineString = GeoLineString <$> arbitrary
            multiLineString = GeoMultiLineString <$> fmap fromList arbitrary
            polygon = GeoPolygon <$> arbitrary
            multiPolygon = GeoMultiPolygon <$> fmap fromList arbitrary
            geometryCollection = GeoCollection . fromList <$> listOf (oneof geometries)
            geometries = [ point, multiPoint, lineString, multiLineString
                         , polygon, multiPolygon ]
