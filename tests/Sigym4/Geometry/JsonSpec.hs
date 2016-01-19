{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, DataKinds #-}

module Sigym4.Geometry.JsonSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Sigym4.Geometry
import Sigym4.Geometry.Json (encode, eitherDecode)
import Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sigym4.Geometry.Json" $ do

    describe "Geometry V2" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V2) -> (eitherDecode . encode $ g) == Right g

    describe "Geometry V3" $ do
      prop "deserializes the same thing it serializes" $
        \(g :: Geometry V3) -> (eitherDecode . encode $ g) == Right g
