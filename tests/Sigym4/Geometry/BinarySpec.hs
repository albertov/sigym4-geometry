{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module Sigym4.Geometry.BinarySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Sigym4.Geometry (Geometry, V2, V3)
import Sigym4.Geometry.Binary (ByteOrder(..), wkbEncode, wkbDecode)
import Arbitrary ()

main :: IO ()
main = hspec spec

instance Arbitrary ByteOrder where
    arbitrary = elements [LittleEndian, BigEndian]

spec :: Spec
spec = do
  describe "Sigym4.Geometry.Binary" $ do
    describe "Geometry V2" $ do
      xprop "deserializes the same thing it serializes" $ \(g :: Geometry V2, bo) ->
        (wkbDecode . wkbEncode bo $ g) == Right g
    describe "Geometry V3" $ do
      xprop "deserializes the same thing it serializes" $ \(g :: Geometry V3, bo) ->
        (wkbDecode . wkbEncode bo $ g) == Right g

xprop _ _ = return ()
