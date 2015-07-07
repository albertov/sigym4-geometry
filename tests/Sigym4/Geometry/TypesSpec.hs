{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , OverloadedStrings
           , DataKinds #-}

module Sigym4.Geometry.TypesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Sigym4.Geometry
import Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HasOffset" $ do
    describe "RowMajor" $ do
      describe "V2" $ do
        prop "toOffset . fromOffset = id" $ \(s, o :: Offset RowMajor) ->
          betweenV2 s o ==> 
            maybe False ((==Just o) . toOffset s) (fromOffset s o)
      describe "V3" $ do
        prop "toOffset . fromOffset = id" $ \(s, o :: Offset RowMajor) ->
          betweenV3 s o ==> 
            maybe False ((==Just o) . toOffset s) (fromOffset s o)
    describe "ColumnMajor" $ do
      describe "V2" $ do
        prop "toOffset . fromOffset = id" $ \(s, o :: Offset ColumnMajor) ->
          betweenV2 s o ==> 
            maybe False ((==Just o) . toOffset s) (fromOffset s o)
      describe "V3" $ do
        prop "toOffset . fromOffset = id" $ \(s, o :: Offset ColumnMajor) ->
          betweenV3 s o ==> 
            maybe False ((==Just o) . toOffset s) (fromOffset s o)

betweenV2 :: Size V2 -> Offset t -> Bool
betweenV2 s o = 0 <= unOff o && unOff o < sx*sy
  where V2 sx sy = unSize s

betweenV3 :: Size V3 -> Offset t -> Bool
betweenV3 s o = 0 <= unOff o && unOff o < sx*sy*sz
  where V3 sx sy sz = unSize s
