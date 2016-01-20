{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , OverloadedStrings
           , DataKinds #-}

module Sigym4.Geometry.BinarySpec (main, spec) where

import Control.Monad
import Data.Maybe (fromJust)
import System.IO hiding (hGetContents)
import Data.ByteString.Lazy (hGetContents)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Sigym4.Geometry
import Sigym4.Geometry.Binary (ByteOrder(..), wkbEncode, wkbDecode)
import Arbitrary ()

main :: IO ()
main = hspec spec

instance Arbitrary ByteOrder where
    arbitrary = elements [LittleEndian, BigEndian]

instance Arbitrary Crs where
  arbitrary = oneof [
    liftM  (fromJust . epsgCrs . getPositive) arbitrary, return noCrs]

spec :: Spec
spec = do
  describe "Sigym4.Geometry.Binary" $ do

    describe "Geometry V2" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V2, bo) ->
          (wkbDecode . wkbEncode bo $ g) == Right g

    describe "WithCrs (Geometry V2)" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V2, bo, crs) ->
          let g' = WithCrs crs g
          in (wkbDecode . wkbEncode bo $ g') == Right g'

    describe "Geometry V3" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V3, bo) ->
          (wkbDecode . wkbEncode bo $ g) == Right g

    describe "wkbDecode" $ do
        it "can decode a postgis wkb dump" $ do
             bs <- hGetContents =<<
                    openFile "tests/fixtures/big_geom.wkb" ReadMode
             case wkbDecode bs :: Either String (WithCrs (Geometry V2)) of
               Right (WithCrs crs _) -> crs `shouldBe` noCrs
               Left _ -> expectationFailure "could not decode postgis dump"
