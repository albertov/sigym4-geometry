{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module Sigym4.Geometry.BinarySpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Sigym4.Geometry
import Sigym4.Geometry.Binary (ByteOrder(..), wkbEncode, wkbDecode)

import Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sigym4.Geometry.Binary" $ do
    describe "2D Point" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry Point V2) -> Bool)
    describe "3D Point" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry Point V3) -> Bool)

    describe "2D MultiPoint" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry MultiPoint V2) -> Bool)
    describe "3D MultiPoint" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry MultiPoint V3) -> Bool)

    describe "2D LineString" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry LineString V2) -> Bool)
    describe "3D LineString" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry LineString V3) -> Bool)

    describe "2D MultiLineString" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry MultiLineString V2) -> Bool)
    describe "3D MultiLineString" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry MultiLineString V3) -> Bool)

    describe "2D Polygon" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry Polygon V2) -> Bool)
    describe "3D Polygon" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry Polygon V3) -> Bool)

    describe "2D MultiPolygon" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry MultiPolygon V2) -> Bool)
    describe "3D MultiPolygon" $ do
      prop "deserializes the same thing it serializes" $
        (encodeDecodeIsId :: (ByteOrder, Geometry MultiPolygon V3) -> Bool)

    {-
    describe "2D GeometryCollection" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry GeometryCollection V2) -> Bool)
    describe "3D GeometryCollection" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry GeometryCollection V3) -> Bool)
    -}

encodeDecodeIsId (bo,o) = (wkbDecode . wkbEncode bo $ o) == Right o
