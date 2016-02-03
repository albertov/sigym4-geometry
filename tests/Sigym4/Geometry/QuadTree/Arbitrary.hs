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
import Control.DeepSeq (NFData(..))
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

type ExtentAndPoints v =
  (QuadTree v NoCrs (Extent v NoCrs), Point v NoCrs, Point v NoCrs)

newtype RandomQT v = RandomQT {unRQt :: ExtentAndPoints v} deriving Show

deriving instance VectorSpace v => NFData (RandomQT v)

newtype DelicateQT v = DelicateQT {unDelQt :: ExtentAndPoints v} deriving Show

deriving instance VectorSpace v => NFData (DelicateQT v)

newtype ProbablyInvalidPointsQT v
  = ProbablyInvalidPointsQT {unPiQt :: ExtentAndPoints v} deriving Show

deriving instance VectorSpace v => NFData (ProbablyInvalidPointsQT v)

randomOrDelicate
  :: Either (RandomQT v) (DelicateQT v) -> ExtentAndPoints v
randomOrDelicate = either unRQt unDelQt

randomOrDelicateOrProbablyInvalid
  :: Either (RandomQT v) (Either (DelicateQT v) (ProbablyInvalidPointsQT v))
  -> ExtentAndPoints v
randomOrDelicateOrProbablyInvalid = either unRQt (either unDelQt unPiQt)

instance VectorSpace v => Arbitrary (LocCode v) where
  arbitrary = (LocCode . unsafeFromCoords) <$> replicateM n arbitrary
    where n = dim (Proxy :: Proxy v)

randomQtOfLevel :: VectorSpace v => Level -> Gen (Either QtError (RandomQT v))
randomQtOfLevel level = do
  ext <- arbitrary
  eQt <- generate2 (randomBuild 0.3) ext level
  case eQt of
    Right qt -> do
      p  <- randomPointInside (qtExtent qt)
      p1  <- randomPointInside (qtExtent qt)
      return (Right (RandomQT (qt, p,p1)))
    Left e -> return (Left e)
  where

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

instance VectorSpace v => Arbitrary (RandomQT v) where
  arbitrary = either (const arbitrary) return =<< randomQtOfLevel =<< arbitrary

instance VectorSpace v => Arbitrary (DelicateQT v) where
  arbitrary = do
    RandomQT (qt,_,_) <- arbitrary

    let candidates = take 100
                   . map Point
                   . concat
                   . map (extentCorners . ensureInQt)
                   $ qtExtent qt : toList qt
        ensureInQt (Extent lo hi) = go (finiteBitSize (undefined :: Word))
          where go !l | qtContainsPoint qt (Point v) = Extent lo v
                      | Level l > qtLevel qt = go (l-1)
                      | otherwise            = error "calculating effectiveMax"
                  where v = hi - calculateMinBox (qtExtent qt) (Level l)
    p1 <- elements candidates
    p2 <- elements candidates
    return (DelicateQT (qt, p1, p2))

instance VectorSpace v => Arbitrary (ProbablyInvalidPointsQT v) where
  arbitrary = do
    RandomQT (qt,_,_) <- arbitrary
    let candidates = take 100
                   . map Point
                   . concat
                   . map extentCorners
                   $ qtExtent qt : toList qt
    p1 <- elements candidates
    p2 <- elements candidates
    return (ProbablyInvalidPointsQT (qt, p1, p2))

epsilon :: Double
epsilon = 1e-6

instance MonadFix Gen where
  mfix f = MkGen (\r n -> let MkGen f'=f v; v=f' r n in v)
