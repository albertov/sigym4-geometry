{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Sigym4.GeometrySpec (main, spec) where

import Test.Hspec (Spec, hspec, parallel, describe, shouldBe, it)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, forAll)

import Control.Applicative (liftA2)
import Arbitrary (positiveV)
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

  describe "northUpGeoTransform" $ do
    prop "preserves origin and has diagonal matrix" $ 
      \(e :: Extent V2 0, s :: Size V2) ->
        case northUpGeoTransform e s of
          Right (GeoTransform (V2 (V2 dx ry) (V2 rx dy)) (V2 x0 y0)) ->
            let V2 dx' dy' = eSize e / fmap fromIntegral (unSize s)
                V2 x0' _ = eMin e
                V2 _ y0' = eMax e
            in rx == 0 &&
               ry == 0 &&
               dx == dx' &&
               dy == (-dy') &&
               x0 == x0' &&
               y0 == y0'
          Left _ -> False

    let genInvalidExtent = do
          lr <- arbitrary
          d <- positiveV
          return $ Extent lr (lr-d)
        genInvalidSize = do
          d <- positiveV
          return $ Size $ (fmap (negate . floor) d)
        isLeft (Left _) = True
        isLeft _        = False

    prop "invalid extent returns Left" $
      forAll (liftA2 (,) genInvalidExtent arbitrary) $
        \(e :: Extent V2 0, s :: Size V2) -> isLeft (northUpGeoTransform e s)

    prop "invalid size returns Left" $
      forAll (liftA2 (,) arbitrary genInvalidSize) $
        \(e :: Extent V2 0, s :: Size V2) -> isLeft (northUpGeoTransform e s)
