{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sigym4.Geometry.QuadTreeSpec (main, spec) where

import Data.List
import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Sigym4.Geometry
import Sigym4.Geometry.QuadTree
import Sigym4.Geometry.Algorithms
import qualified Data.Semigroup as SG
import Data.List.NonEmpty (fromList)
import Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "QuadTree" $ do

  describe "innerExtent" $ do

    prop "is smaller" $ \(q, ext) ->
      eSize (innerExtent q ext) < eSize ext

    prop "concatenation of subextents original extent" $ \ext ->
      SG.sconcat (fromList (map (flip innerExtent ext) [minBound..maxBound]))
        == ext

    prop "subextents are disjoint" $ \ext ->
      let subExts = map (flip innerExtent ext) [minBound..maxBound]
          pairs = [(a,b) | (a:b:_) <- permutations subExts]
      in all (not . uncurry contains) pairs

  describe "outerExtent" $ do

    prop "is bigger" $ \(q, ext) ->
      eSize (outerExtent q ext) > eSize ext

    prop "outer contains original" $ \(q, ext) ->
      outerExtent q ext `contains` ext

    prop "original is a subextent of outer" $ \(q,ext) ->
      innerExtent q (outerExtent q ext) `almostEqExt` ext

almostEqExt :: Extent V2 t -> Extent V2 t1 -> Bool
almostEqExt (Extent (V2 a1 a2) (V2 a3 a4)) (Extent (V2 b1 b2) (V2 b3 b4))
  =  abs (a1-b1) <= epsilon && abs (a2-b2) <= epsilon
  && abs (a3-b3) <= epsilon && abs (a4-b4) <= epsilon
  where epsilon = 1e-6
