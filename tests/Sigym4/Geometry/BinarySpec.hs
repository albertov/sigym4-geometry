{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , OverloadedStrings
           , DataKinds #-}

module Sigym4.Geometry.BinarySpec (main, spec) where

import System.IO hiding (hGetContents)
import Data.ByteString.Lazy (hGetContents)
import Data.Either (isRight)
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
      prop "deserializes the same thing it serializes" $ \(g :: Geometry V2 0, bo) ->
        (wkbDecode . wkbEncode bo $ g) == Right g
    describe "Geometry V3" $ do
      prop "deserializes the same thing it serializes" $ \(g :: Geometry V3 0, bo) ->
        (wkbDecode . wkbEncode bo $ g) == Right g
    describe "wkbDecode" $ do
        it "can decode a postgis wkb dump" $ do
             bs <- hGetContents =<< openFile "tests/fixtures/big_geom.wkb" ReadMode
             let rGeom = wkbDecode bs :: Either String (Geometry V2 0)
             isRight rGeom `shouldBe` True


            
