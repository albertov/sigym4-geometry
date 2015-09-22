{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sigym4.Geometry.QuadTreeSpec (main, spec) where

import Data.List
import Data.Functor.Identity (runIdentity)
import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (generate)
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

  describe "qtMinBox" $ do
    
    prop "qtMinBox == eSize qtExtent for level 0" $ \ext ->
      let q = runIdentity (generate (Leaf ()) ext 0)
      in qtMinBox q == eSize (qtExtent q)

    prop "qtMinBox is constant after grows" $ \(dir,ext,l) ->
      let q = runIdentity (generate (Leaf ()) ext 0)
          q2 = foldr (const growIt) q [1..n]
          Level n = l
          growIt = runIdentity . grow (Leaf ()) dir
      in qtMinBox q2 `almostEqVertex` qtMinBox q && qtLevel q2 == l

  describe "lookupByPoint" $ do

    prop "can lookup point inside in a leaf" $ \(EQT (qt,p)) -> 
      case lookupByPoint qt p of
        Nothing      -> False
        Just (e1,e2) -> e1 `almostEqExt` e2 && e1 `contains` p

    prop "can lookup any point" $ \(EQT (qt,_), p) -> 
      case lookupByPoint qt p of
        Nothing      -> not (qtExtent qt `contains` p)
        Just (e1,e2) -> e1 `almostEqExt` e2 && e1 `contains` p

almostEqExt :: Extent V2 t -> Extent V2 t1 -> Bool
almostEqExt (Extent a0 a1) (Extent b0 b1)
  =  a0 `almostEqVertex` b0 && a1 `almostEqVertex` b1

almostEqVertex :: Vertex V2 -> Vertex V2 -> Bool
almostEqVertex (V2 a1 a2) (V2 b1 b2)
  =  abs (a1-b1) <= epsilon && abs (a2-b2) <= epsilon
  where epsilon = 1e-6

newtype ExtQuadTree = EQT (QuadTree 0 (Extent V2 0), Point V2 0)
  deriving Show

instance Arbitrary ExtQuadTree where
  arbitrary = do
    ext <- arbitrary
    level <- Level <$> choose (0,5)
    qt <- generate build ext level
    p  <- genPoint ext
    return (EQT (qt, p))
    where
      epsilon = 1e-6
      build = Node $ \ext -> do
        doLeaf <- fmap (> 80) (choose (0,100) :: Gen Int)
        if doLeaf
          then return (ext, Leaf ext)
          else return (ext, build)
      genPoint (Extent (V2 x0 y0) (V2 x1 y1)) = do
        x <- choose (x0, x1-epsilon)
        y <- choose (y0, y1-epsilon)
        return (Point (V2 x y))
