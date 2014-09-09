{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Arbitrary where

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import Data.Vector.Unboxed as U (fromList)
import Data.Vector as V (fromList)
import Data.Typeable (Typeable)

import Sigym4.Geometry
import Sigym4.Geometry.Binary (ByteOrder(..))

instance Arbitrary ByteOrder where
  arbitrary = elements [NDR, XDR]

instance Arbitrary t => Arbitrary (V2 t) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (V3 t) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry Point v) where
    arbitrary = fmap MkPoint arbitrary

instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry MultiPoint v) where
    arbitrary = fmap (MkMultiPoint . V.fromList) arbitrary

instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry LineString v) where
    arbitrary = fmap MkLineString arbitrary

instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry MultiLineString v) where
    arbitrary = fmap (MkMultiLineString . V.fromList) arbitrary


instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (LinearRing v) where
    arbitrary = fmap U.fromList arbitrary

instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry Polygon v) where
    arbitrary = fmap (MkPolygon . V.fromList) arbitrary

instance (Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry MultiPolygon v) where
    arbitrary = fmap (MkMultiPolygon . V.fromList) arbitrary

instance (Typeable v, Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry GeometryCollection v) where
    arbitrary = fmap (MkGeometryCollection . V.fromList) arbitrary

instance forall v. (Typeable v, Arbitrary (v Double), IsVertex v Double) =>
  Arbitrary (Geometry AnyGeometry v) where
    arbitrary = do
      type_ <- arbitrary
      case type_ of
        Point -> toAnyGeometry <$> (arbitrary :: Gen (Geometry Point v))
        MultiPoint -> toAnyGeometry <$> (arbitrary :: Gen (Geometry MultiPoint v))
        LineString -> toAnyGeometry <$> (arbitrary :: Gen (Geometry LineString v))
        MultiLineString -> toAnyGeometry <$> (arbitrary :: Gen (Geometry MultiLineString v))
        Polygon -> toAnyGeometry <$> (arbitrary :: Gen (Geometry Polygon v))
        MultiPolygon -> toAnyGeometry <$> (arbitrary :: Gen (Geometry MultiPolygon v))

instance Arbitrary GeometryType where
  arbitrary = elements $ [Point,MultiPoint,LineString,MultiLineString,Polygon,MultiPolygon]
