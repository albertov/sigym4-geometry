{-# LANGUAGE ScopedTypeVariables #-}
module Sigym4.GeometrySpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Arbitrary ()
import Sigym4.Geometry

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "toOffset" $ do
    describe "on V2" $ do
      describe "RowMajor" $ do
        prop "produces an offset >= 0 and < w*h" $
          \(s@(Size (V2 sx sy)), p@(Pixel (V2 px py))) -> 
          case toOffset s p of
            Just (Offset o :: Offset RowMajor) -> o>=0 && o<sx*sy
            Nothing                            -> px<0
                                               || py<0
                                               || px >= fromIntegral sx
                                               || py >= fromIntegral sy
        it "produces a valid result" $ do
          toOffset (Size (V2 5 4)) (Pixel (V2 2 3)) `shouldBe` Just (Offset 17 :: Offset RowMajor)
          
           
      describe "ColumnMajor" $ do
        prop "produces an offset >= 0 and < w*h" $
          \(s@(Size (V2 sx sy)), p@(Pixel (V2 px py))) -> 
          case toOffset s p of
            Just (Offset o :: Offset ColumnMajor) -> o>=0 && o<sx*sy
            Nothing                               -> px<0
                                                  || py<0
                                                  || px >= fromIntegral sx
                                                  || py >= fromIntegral sy
        it "produces a valid result" $ do
          toOffset (Size (V2 5 4)) (Pixel (V2 2 3)) `shouldBe` Just (Offset 11 :: Offset ColumnMajor)

    describe "on V3" $ do
      describe "RowMajor" $ do
        prop "produces an offset >= 0 and < w*h*z" $
          \(s@(Size (V3 sx sy sz)), p@(Pixel (V3 px py pz))) -> 
          case toOffset s p of
            Just (Offset o :: Offset RowMajor) -> o>=0 && o<sx*sy*sz
            Nothing                            -> px<0
                                               || py<0
                                               || pz<0
                                               || px >= fromIntegral sx
                                               || py >= fromIntegral sy
                                               || pz >= fromIntegral sz
        it "produces a valid result" $ do
          toOffset (Size (V3 5 4 2)) (Pixel (V3 2 3 1)) `shouldBe` Just (Offset 37 :: Offset RowMajor)

      describe "ColumnMajor" $ do
        prop "produces an offset >= 0 and < w*h*z" $
          \(s@(Size (V3 sx sy sz)), p@(Pixel (V3 px py pz))) -> 
          case toOffset s p of
            Just (Offset o :: Offset ColumnMajor) -> o>=0 && o<sx*sy*sz
            Nothing                               -> px<0
                                                  || py<0
                                                  || pz<0
                                                  || px >= fromIntegral sx
                                                  || py >= fromIntegral sy
                                                  || pz >= fromIntegral sz

        it "produces a valid result" $ do
          toOffset (Size (V3 5 4 2)) (Pixel (V3 2 3 1)) `shouldBe` Just (Offset 23 :: Offset ColumnMajor)
