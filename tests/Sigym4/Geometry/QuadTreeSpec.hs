{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Sigym4.Geometry.QuadTreeSpec (main, spec) where

import Control.Monad (when)
import Data.Either (isRight)
import Data.Proxy
import Data.Foldable (toList)
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


quadTreeSpec
  :: forall v. ( HasHyperplanes v
               , Show (v Half)
               , Show (v Int)
               , Num (v Int)
               , Eq (v Int)
               , Show (v NeighborDir)
               , Show (HyperPlaneDirections v)
               )
  => String -> Proxy v -> Spec
quadTreeSpec msg _ = describe ("QuadTree " ++ msg) $ do

  describe "setChildBits and quadrantAtLevel " $ do
    prop "are reciprocal" $ \(level, q) ->
      quadrantAtLevel level (setChildBits level q (0::LocCode v)) == q

  describe "neighborsDefault " $ do
    it " has correct length" $
      length (neighborsDefault :: Neighbors v) == 3^(dim (Proxy :: Proxy v)) - 1

  describe "is a functor" $ do
    prop "fmap id = id" $ \arg ->
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in qtLevel qt <= Level 3 ==> -- else it takes too long
        fmap id qt == id qt

    prop "fmap (g . f)  = fmap g . fmap f" $ \arg ->
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in qtLevel qt <= Level 3 ==> -- else it takes too long
         fmap (maximum . eSize) qt == fmap maximum (fmap eSize qt)

  describe "is Traversable" $ do
    prop "mapM return = id" $ \arg ->
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in qtLevel qt <= Level 3 ==> -- else it takes too long
         qt == runIdentity (mapM return qt)

  describe "is Foldable" $ do
    prop "length is correct" $ \(Level l, ext::Extent v srid) ->
      l <= 3 ==>
        let gen = Node (\e -> return (e, gen))
            Right qt = runIdentity (generate2 gen ext (Level l))
        in length qt == (2 ^ dim (Proxy :: Proxy v)) ^ l

    prop "length of toList is correct" $ \(Level l, ext::Extent v srid) ->
      l <= 3 ==>
        let gen = Node (\e -> return (e, gen))
            Right qt = runIdentity (generate2 gen ext (Level l))
        in length (toList qt) == (2 ^ dim (Proxy :: Proxy v)) ^ l


  describe "qtMinBox" $ do

    prop "qtMinBox == eSize qtExtent for level 0" $ \(ext :: Extent v srid) ->
      let eq      = runIdentity (generate2 (Leaf ()) ext minBound)
          Right q = eq
      in isRight eq ==> qtMinBox q == eSize (qtExtent q)

    prop "qtMinBox is constant after grows" $ \( dirs :: [Quadrant v]
                                               , ext  :: Extent v srid
                                               ) ->
      let eq = runIdentity (generate2 (Leaf ()) ext minBound)
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

    prop "can lookup any point" $ \(arg, p :: Point v NoCrs) ->
      let RandomQT (qt,_,_) = arg :: RandomQT v
      in case lookupByPoint qt p of
          Nothing  -> not (qtExtent qt `contains` p)
          Just e   -> e `contains` p

  describe "growToInclude" $ do
    let qtAndPointOutside = do
          e :: Extent v NoCrs <- arbitrary
          se <- randomSuperExtent e
          p <- randomPointInside se
          if e `contains` p then qtAndPointOutside else do
            level <- arbitrary
            eQt <- generate2 (randomBuild 0.3) e level
            case eQt of
              Right qt -> return (qt,p)
              Left _   -> qtAndPointOutside
    prop "if it grows it includes point" $
      forAll qtAndPointOutside $ \(qt, p) ->
        let eQt = runIdentity (growToInclude build p qt)
            build = Node (\e -> return (e, Leaf e))
        in isRight eQt ==> let Right qt2 = eQt in qt2 `qtContainsPoint` p

  when (dim (Proxy :: Proxy v) <= 4) $

    describe "traceRay" $ do

      prop "does not get get in a loop, fail assertions or throw errors" $
        \arg ->
            let qps :: ExtentAndPoints v
                qps       = randomOrDelicateOrProbablyInvalid arg
                (qt,p,p1) = qps
                els       = traceRay qt p p1
                notSameExt a b
                  | a == b    = traceShow ("repeated", a, b) False
                  | otherwise = True
            in length (take 2 els) > 1 ==>
                 all id (zipWith notSameExt els (tail els))

      prop "a ray from A to A belonging to Qtree returns a singleton" $
        \arg -> let (qt,p,_) = randomOrDelicate arg :: ExtentAndPoints v
                in case (traceRay qt p p, qtContainsPoint qt p) of
                     ([], True)  -> error "did not return any els in test"
                     ([], False) -> error "generating invalid delicate points"
                     ((a:[]), True)
                        | Just a == lookupByPoint qt p             -> True
                        | traceShow ("a->a",qtExtent qt, a,p) True -> False
                     _           -> error "traceRay returns junk"

      prop "last contains to" $
        \arg -> let (qt,p,p1) = randomOrDelicate arg :: ExtentAndPoints v
                in case ( traceRay qt p p1
                        , qtContainsPoint qt p && qtContainsPoint qt p1) of
                     ([], True)  -> error "did not return any els in test"
                     ([], False) -> error "generating invalid delicate points"
                     (_, False) -> error "traceRay returns junk"
                     (els, True)
                        | Just (last els) == lookupByPoint qt p1  -> True
                        | traceShow ("last",qtExtent qt,last els,p1) True -> False
                     _           -> error "traceRay returns junk"

      prop "if from and to are in qtree then head contains from" $
        \arg -> let qps :: ExtentAndPoints v
                    qps       = randomOrDelicateOrProbablyInvalid arg
                    (qt,p,p1) = qps
                in qtContainsPoint qt p && qtContainsPoint qt p1 ==>
                   case traceRay qt p p1 of
                     []  -> error "did not return any els in property test"
                     els -> Just (head els) == lookupByPoint qt p
