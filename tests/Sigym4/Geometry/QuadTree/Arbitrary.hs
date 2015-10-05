{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Sigym4.Geometry.QuadTree.Arbitrary where

import Control.Monad.Fix
import Control.Monad (replicateM, zipWithM)
import Data.Foldable (toList)

import Data.Proxy (Proxy(Proxy))

import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Gen (Gen(MkGen))

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree.Internal.Types
import Sigym4.Geometry.QuadTree.Internal.Algorithms

import Debug.Trace

import Arbitrary ()

instance VectorSpace v => Arbitrary (Quadrant v) where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Level where
  arbitrary = fromInteger <$> choose (lo, hi)
    where lo = fromIntegral (unLevel minBound)
          hi = fromIntegral (unLevel maxBound)

type ExtentAndPoints v = (QuadTree v 0 (Extent v 0), Point v 0, Point v 0)

newtype RandomQT v = RandomQT {unRQt :: ExtentAndPoints v} deriving Show

newtype DelicateQT v = DelicateQT {unDelQt :: ExtentAndPoints v} deriving Show

newtype ProbablyInvalidPointsQT v
  = ProbablyInvalidPointsQT {unPiQt :: ExtentAndPoints v} deriving Show

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

instance VectorSpace v => Arbitrary (RandomQT v) where
  arbitrary = do
    ext <- arbitrary
    level <- Level <$> choose (unLevel minBound, unLevel maxBound)
    eQt <- generate build ext level
    case eQt of
      Right qt -> do
        p  <- genPointInside qt
        p1  <- genPointInside qt
        return (RandomQT (qt, p,p1))
      Left _ -> arbitrary
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

instance VectorSpace v => Arbitrary (DelicateQT v) where
  arbitrary = do
    RandomQT (qt,_,_) <- arbitrary

    let candidates = take 100
                   . map Point
                   . concat
                   . map (extentCorners . ensureInQt)
                   $ qtExtent qt : toList qt
        ensureInQt (Extent lo hi) = go maxBound
          where go !l | qtContainsPoint qt (Point v) = Extent lo v
                      | l > qtLevel qt       = go (l-1)
                      | otherwise            = error "calculating effectiveMax"
                  where v = hi - calculateMinBox (qtExtent qt) l
    p1 <- elements candidates
    p2 <- elements (filter (/=p1) candidates)
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
