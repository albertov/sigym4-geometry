{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Sigym4.Geometry.QuadTreeSpec (main, spec) where

import Control.Applicative (liftA2)
import Control.Monad (when, zipWithM)
import Control.Monad.Fix
import Data.List
import Data.List.NonEmpty (fromList)
import Data.Proxy
import qualified Data.Foldable as F
import Data.Functor.Identity (runIdentity)
import Test.Hspec (Spec, hspec, describe, it)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Gen (Gen(MkGen))
import qualified Data.Semigroup as SG
import GHC.TypeLits

import Arbitrary ()

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree
import Sigym4.Geometry.QuadTree.Internal.Algorithms

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  quadTreeSpec "V2" (Proxy :: Proxy V2)
  quadTreeSpec "V3" (Proxy :: Proxy V3)
  quadTreeSpec "V4" (Proxy :: Proxy V4)
  quadTreeSpec "V5" (Proxy :: Proxy (V 5))
  quadTreeSpec "V6" (Proxy :: Proxy (V 6))


quadTreeSpec
  :: forall v. (VectorSpace v, Show (v Half), KnownNat (VsDim v - 1), Show (v Word), Eq (v Word), Show (v NeighborDir))
  => String -> Proxy v -> Spec
quadTreeSpec msg _ = describe ("QuadTree " ++ msg) $ do

  describe "neighbors " $ do
    it " has correct length" $ 
      length (neighbors :: Neighbors v) == 3^(dim (Proxy :: Proxy v)) - 1

  describe "innerExtent " $ do
    prop "is smaller" $ \(q, (ext :: Extent v srid)) ->
      liftBinBool (<) (eSize (innerExtent q ext)) (eSize ext)

    prop "concatenation of subextents original extent" $
      \(ext :: Extent v srid) ->
        let subs = map (flip innerExtent ext) [minBound..maxBound]
        in SG.sconcat (fromList subs) == ext

    -- Takes a random sample because !(2^4) is quite large
    prop "subextents are disjoint" $ \(ext :: Extent v srid, lo, hi) ->
      fromEnum lo < fromEnum hi ==>
        let subs = map (flip innerExtent ext) [lo..hi]
            pairs = [(a,b) | (a:b:_) <- permutations subs]
        in all (not . uncurry contains) (take 20 pairs)

  describe "outerExtent" $ do

    prop "is bigger" $ \(q, (ext :: Extent v srid)) ->
      liftBinBool (>) (eSize (outerExtent q ext)) (eSize ext)

    prop "outer contains original" $ \(q, (ext :: Extent v srid)) ->
      outerExtent q ext `contains` ext

    prop "original is a subextent of outer" $ \(q, (ext :: Extent v srid)) ->
      innerExtent q (outerExtent q ext) `almostEqExt` ext

  describe "qtMinBox" $ do
    
    prop "qtMinBox == eSize qtExtent for level 0" $ \(ext :: Extent v srid) ->
      let q = runIdentity (generate (Leaf ()) ext 0)
      in qtMinBox q == eSize (qtExtent q)

    prop "qtMinBox is constant after grows" $ \(dir,(ext :: Extent v srid),l) ->
      let q = runIdentity (generate (Leaf ()) ext 0)
          q2 = foldr (const growIt) q ([1..n] :: [Int])
          n = unLevel l
          growIt = runIdentity . grow (Leaf ()) dir
      in qtMinBox q2 `almostEqVertex` qtMinBox q && qtLevel q2 == l

  describe "lookupByPoint" $ do

    prop "can lookup point inside in a leaf" $ \((EQT (qt,p,_) :: EQT v)) -> 
      case lookupByPoint qt p of
        Nothing      -> False
        Just (e1,e2) -> e1 `almostEqExt` e2 && e1 `contains` p

    prop "can lookup any point" $ \((EQT (qt,_,_) :: EQT v), p :: Point v 0) -> 
      case lookupByPoint qt p of
        Nothing      -> not (qtExtent qt `contains` p)
        Just (e1,e2) -> e1 `almostEqExt` e2 && e1 `contains` p

  when (dim (Proxy :: Proxy v) <= 4) $
    describe "traceRay" $ do
      prop "does not repeat elements" $
        \(EQT (qt,p,p1) :: EQT v) ->
            let els = traceRay qt p p1
            in not (any (uncurry almostEqExt) (zip els (tail els)))
               || length (take 2 els) == 1

      prop "last contains to" $
        \(EQT (qt,p,p1) :: EQT v) ->
            last (traceRay qt p p1) `contains` p1
            


almostEqExt :: VectorSpace v => Extent v t -> Extent v t -> Bool
almostEqExt (Extent a0 a1) (Extent b0 b1)
  =  a0 `almostEqVertex` b0 && a1 `almostEqVertex` b1

liftBinBool f a =  F.all id . liftA2 f a

newtype EQT v = EQT (QuadTree v 0 (Extent v 0), Point v 0, Point v 0)
  deriving Show

instance VectorSpace v => Arbitrary (EQT v) where
  arbitrary = do
    ext <- arbitrary
    level <- fromInteger <$> choose (0,5)
    qt <- generate build ext level
    p  <- genPoint ext
    p1  <- genPoint ext
    return (EQT (qt, p,p1))
    where
      epsilon = 1e-6
      build = Node $ \ext -> do
        doLeaf <- fmap (> 30) (choose (0,100) :: Gen Int)
        if doLeaf
          then return (ext, Leaf ext)
          else return (ext, build)
      genPoint (Extent lo hi)
        = fmap (Point . unsafeFromCoords)
               (zipWithM (\a b -> choose (a,b-epsilon)) (coords lo) (coords hi))

instance MonadFix Gen where
  mfix f = MkGen (\r n -> let MkGen f'=f v; v=f' r n in v)
