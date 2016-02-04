{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sigym4.Geometry.QuadTree.Arbitrary where

import Control.Monad.Fix
import Control.Monad (replicateM, zipWithM, liftM)
import Data.Foldable (toList)

import Data.Bits (finiteBitSize)
import Data.Proxy (Proxy(Proxy))

import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Gen (Gen(MkGen))

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree.Internal.Types
import Sigym4.Geometry.QuadTree.Internal.Algorithms

import Arbitrary ()

instance VectorSpace v => Arbitrary (Quadrant v) where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Level where
  arbitrary = Level <$> choose (lo, hi)
    where lo = fromIntegral (unLevel minBound)
          hi = fromIntegral (unLevel maxBound)

instance VectorSpace v => Arbitrary (LocCode v) where
  arbitrary = (LocCode . unsafeFromCoords) <$> replicateM n arbitrary
    where n = dim (Proxy :: Proxy v)

type QtAndPoints v =
  (QuadTree v NoCrs (Extent v NoCrs), Point v NoCrs, Point v NoCrs)

qtAndPointsInside :: VectorSpace v => Gen (QtAndPoints v)
qtAndPointsInside = do
  qt <- randomQt
  (,,) <$> pure qt
       <*> randomPointInside (qtExtent qt)
       <*> randomPointInside (qtExtent qt)

qtAndPointsOutsideButNear :: VectorSpace v => Gen (QtAndPoints v)
qtAndPointsOutsideButNear = do
  qt <- randomQt
  (,,) <$> pure qt
       <*> randomPointOutsideButNear (qtExtent qt)
       <*> randomPointOutsideButNear (qtExtent qt)


qtAndPointsCloseToEdges :: VectorSpace v => Gen (QtAndPoints v)
qtAndPointsCloseToEdges = do
  qt <- randomQt
  (,,) <$> pure qt
       <*> pointCloseToEdges qt
       <*> pointCloseToEdges qt

pointCloseToEdges
  :: VectorSpace v
  => QuadTree v crs (Extent v crs)
  -> Gen (Point v crs)
pointCloseToEdges qt
  = elements
  . take 100
  . map Point
  . concat
  . map (extentCorners . ensureInQt)
  $ qtExtent qt : toList qt
  where
    ensureInQt (Extent lo hi) = go (finiteBitSize (undefined :: Word))
        where go !l | qtContainsPoint qt (Point v) = Extent lo v
                    | Level l > qtLevel qt = go (l-1)
                    | otherwise            = error "calculating effectiveMax"
                where v = hi - calculateMinBox (qtExtent qt) (Level l)

qtAndPointsOnEdges :: VectorSpace v => Gen (QtAndPoints v)
qtAndPointsOnEdges = do
  qt <- randomQt
  (,,) <$> pure qt
       <*> pointOnEdges qt
       <*> pointOnEdges qt

pointOnEdges
  :: VectorSpace v
  => QuadTree v crs (Extent v crs)
  -> Gen (Point v crs)
pointOnEdges qt
  = elements
  . take 100
  . map Point
  . concat
  . map extentCorners
  $ qtExtent qt : toList qt

randomQt :: VectorSpace v => Gen (QuadTree v crs (Extent v crs))
randomQt = do
  ext <- arbitrary
  either (const randomQt) return =<< generate2 build ext =<< arbitrary
  where build = randomBuild defaultNodeProbability

defaultNodeProbability :: Double
defaultNodeProbability = 0.3

randomBuild :: VectorSpace v => Double -> Node Gen v crs (Extent v crs)
randomBuild nodeProbability
  | not (0 <= nodeProbability && nodeProbability <= 1) =
    error "randomBuild: nodeProbability must be between 0 and 1"
randomBuild nodeProbability = Node build
  where
    build ext = do
      doLeaf <- fmap (>= nodeProbability) (choose (0,1))
      if doLeaf
        then return (ext, Leaf ext)
        else return (ext, Node build)

randomSuperExtent
  :: forall v crs. VectorSpace v => Extent v crs -> Gen (Extent v crs)
randomSuperExtent ext = go ext =<< choose (1,5)
  where
    go :: Extent v crs -> Int -> Gen (Extent v crs)
    go e n | n<=0 = return e
    go e n        = arbitrary >>= \q -> go (outerExtent q e) (n-1)


randomPointInside :: VectorSpace v => Extent v crs -> Gen (Point v crs)
randomPointInside (Extent lo hi) =
  liftM (Point . unsafeFromCoords)
        (zipWithM (\a b -> choose (a,b)) (coords lo) (coords hi))

randomPointOutsideButNear :: VectorSpace v => Extent v crs -> Gen (Point v crs)
randomPointOutsideButNear e = do
  se <- randomSuperExtent e
  p <- randomPointInside se
  if e `contains` p then randomPointOutsideButNear e else return p

epsilon :: Double
epsilon = 1e-6

instance MonadFix Gen where
  mfix f = MkGen (\r n -> let MkGen f'=f v; v=f' r n in v)
