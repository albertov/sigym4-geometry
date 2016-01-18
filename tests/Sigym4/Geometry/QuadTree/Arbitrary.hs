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
import Control.Monad (replicateM, zipWithM)
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
  eQt <- generate2 build ext level
  case eQt of
    Right qt -> do
      p  <- genPointInside qt
      p1  <- genPointInside qt
      return (Right (RandomQT (qt, p,p1)))
    Left e -> return (Left e)
  where

    build = Node $ \ext -> do
      doLeaf <- fmap (> 30) (choose (0,100) :: Gen Int)
      if doLeaf
        then return (ext, Leaf ext)
        else return (ext, build)

    genPointInside qt@QuadTree{qtExtent=Extent lo hi} = do
      p <- fmap (Point . unsafeFromCoords)
             (zipWithM (\a b -> choose (a,b)) (coords lo) (coords hi))
      if qtContainsPoint qt p
        then return p
        else genPointInside qt

instance VectorSpace v => Arbitrary (RandomQT v) where
  arbitrary = do
    level <- Level <$> choose (unLevel minBound, unLevel maxBound)
    either (const arbitrary) return =<< randomQtOfLevel level

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
