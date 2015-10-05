{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Sigym4.Geometry.QuadTreeSpec (main, spec) where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Data.Either (isRight)
import Data.Proxy
import qualified Data.Foldable as F
import Data.Functor.Identity (runIdentity)
import Test.Hspec (Spec, hspec, describe, it)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (generate)


import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree
import Sigym4.Geometry.QuadTree.Internal.Types
import Sigym4.Geometry.QuadTree.Internal.Algorithms

import Arbitrary ()
import Sigym4.Geometry.QuadTree.Arbitrary

import Debug.Trace

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
  :: forall v. ( HasHyperplanes v
               , Show (v Half)
               , Show (v Word)
               , Num (v Word)
               , Eq (v Word)
               , Show (v NeighborDir)
               , Show (VPlanes v (Direction v))
               )
  => String -> Proxy v -> Spec
quadTreeSpec msg _ = describe ("QuadTree " ++ msg) $ do

  describe "setChildBits and ixFromLocCode " $ do
    prop "are reciprocal" $ \(level, q) ->
      ixFromLocCode level (setChildBits level q (0::LocCode v)) == fromEnum q

  describe "neighbors " $ do
    it " has correct length" $ 
      length (neighbors :: Neighbors v) == 3^(dim (Proxy :: Proxy v)) - 1

  describe "is a functor" $ do
    prop "fmap id = id" $ \arg ->
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in qtLevel qt <= 3 ==> -- else it takes too long
        fmap id qt == id qt

    prop "fmap (g . f)  = fmap g . fmap f" $ \arg ->
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in qtLevel qt <= 3 ==> -- else it takes too long
         fmap (maximum . eSize) qt == fmap maximum (fmap eSize qt)


  describe "qtMinBox" $ do
    
    prop "qtMinBox == eSize qtExtent for level 0" $ \(ext :: Extent v srid) ->
      let eq      = runIdentity (generate (Leaf ()) ext 0)
          Right q = eq
      in isRight eq ==> qtMinBox q == eSize (qtExtent q)

    prop "qtMinBox is constant after grows" $ \( dirs :: [Quadrant v]
                                               , ext  :: Extent v srid
                                               ) ->
      let eq = runIdentity (generate (Leaf ()) ext 0)
          eq2 = foldr growIt eq dirs
          growIt dir (Right q) = runIdentity (grow (Leaf ()) dir q)
          growIt _   l@Left{}  = l
      in Level (length dirs) < maxBound && isRight eq ==>
           case (eq,eq2) of
             (Right q, Right q2) ->
              qtMinBox q2 `almostEqVertex` qtMinBox q &&
              qtLevel q2 == Level (length dirs)
             _   -> False

  describe "lookupByPoint" $ do

    prop "can lookup point inside in a leaf" $ \arg ->
      let RandomQT (qt,p,_) = arg :: RandomQT v
      in case lookupByPoint qt p of
          Nothing  -> False
          Just e   -> e `contains` p

    prop "can lookup any point" $ \(arg, p :: Point v 0) -> 
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in case lookupByPoint qt p of
          Nothing  -> not (qtExtent qt `contains` p)
          Just e   -> e `contains` p

  when (dim (Proxy :: Proxy v) <= 4) $

    describe "traceRay" $ do

      prop "does not get get in a loop, fail assertions or throw errors" $
        \arg ->
            let qps :: ExtentAndPoints v
                qps       = randomOrDelicateOrProbablyInvalid arg
                (qt,p,p1) = qps
                els       = traceRay qt p p1
                notSameExt a b
                  | almostEqExt a b = traceShow ("repeated", a, b) False
                  | otherwise       = True
            in length (take 2 els) > 1 ==>
                 all id (zipWith notSameExt els (tail els))

      prop "a ray from A to A belonging to Qtree returns a singleton" $
        \arg -> let (qt,p,_) = randomOrDelicate arg :: ExtentAndPoints v
                in case (traceRay qt p p, qtContainsPoint qt p) of
                     ([], True)  -> error "did not return any els in test"
                     ([], False) -> error "generating invalid delicate points"
                     ((a:[]), True)
                        | a `contains` p                           -> True
                        | traceShow ("a->a",qtExtent qt, a,p) True -> False
                     _           -> error "traceRay returns junk"

      prop "last contains to" $
        \arg -> let (qt,p,p1) = randomOrDelicate arg :: ExtentAndPoints v
                in case ( traceRay qt p p1
                        , qtContainsPoint qt p && qtContainsPoint qt p1) of
                     ([], True)  -> error "did not return any els in test"
                     ([], False) -> error "generating invalid delicate points"
                     (_, False) -> error "traceRay returns junk"
                     (els, True) -> last els `contains` p1

      prop "if from and to are in qtree then head contains from" $
        \arg -> let qps :: ExtentAndPoints v
                    qps       = randomOrDelicateOrProbablyInvalid arg
                    (qt,p,p1) = qps
                in qtContainsPoint qt p && qtContainsPoint qt p1 ==>
                   case traceRay qt p p1 of
                     []  -> error "did not return any els in property test"
                     els -> head els `contains` p



almostEqExt :: VectorSpace v => Extent v t -> Extent v t -> Bool
almostEqExt (Extent a0 a1) (Extent b0 b1)
  =  a0 `almostEqV` b0 && a1 `almostEqV` b1
  where almostEqV a b = F.all qtNearZero (abs (a-b))
