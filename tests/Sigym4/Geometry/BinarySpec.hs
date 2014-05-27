{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

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
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry Point V2) -> Bool)
    describe "3D Point" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry Point V3) -> Bool)
    describe "2D LineString" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry LineString V2) -> Bool)
    describe "3D LineString" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry LineString V3) -> Bool)
    describe "2D Polygon" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry Polygon V2) -> Bool)
    describe "3D Polygon" $ do
      it "deserializes the same thing it serializes" $ property $
        (encodeDecodeIsId :: (ByteOrder, Geometry Polygon V3) -> Bool)

encodeDecodeIsId (bo,o) = (wkbDecode . wkbEncode bo $ o) == Right o
