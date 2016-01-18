{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, DataKinds #-}

module Sigym4.Geometry.JsonSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Sigym4.Geometry (Geometry, NoCrs, V2, V3)
import Sigym4.Geometry.Json (jsonEncode, jsonDecode)
import Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sigym4.Geometry.Json" $ do
    describe "Geometry V2" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V2 NoCrs) -> (jsonDecode . jsonEncode $ g) == Right g
    describe "Geometry V3" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V3 NoCrs) -> (jsonDecode . jsonEncode $ g) == Right g
