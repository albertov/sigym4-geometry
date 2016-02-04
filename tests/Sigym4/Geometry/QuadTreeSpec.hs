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
quadTreeSpec msg proxy = describe ("QuadTree " ++ msg) $ do

  describe "setChildBits and quadrantAtLevel " $ do
    prop "are reciprocal" $ \(level, q) ->
      quadrantAtLevel level (setChildBits level q (0::LocCode v)) == q

  describe "neighborsDefault " $ do
    it " has correct length" $
      length (neighborsDefault :: Neighbors v) == 3^(dim proxy) - 1

  describe "is a functor" $ do
    prop "fmap id = id" $
      forAll randomQt $ \(qt :: QuadTree v NoCrs (Extent v NoCrs)) ->
        qtLevel qt <= Level 3 ==> -- else it takes too long
          fmap id qt == id qt

    prop "fmap (g . f)  = fmap g . fmap f" $
      forAll randomQt $ \(qt :: QuadTree v NoCrs (Extent v NoCrs)) ->
        qtLevel qt <= Level 3 ==> -- else it takes too long
         fmap (maximum . eSize) qt == fmap maximum (fmap eSize qt)

  describe "is Traversable" $ do
    prop "mapM return = id" $
      forAll randomQt $ \(qt :: QuadTree v NoCrs (Extent v NoCrs)) ->
        qtLevel qt <= Level 3 ==> -- else it takes too long
         qt == runIdentity (mapM return qt)

  describe "is Foldable" $ do
    prop "length is correct" $ \(Level l, ext::Extent v srid) ->
      l <= 3 ==>
        let gen = Node (\e -> return (e, gen))
            Right qt = runIdentity (generate2 gen ext (Level l))
        in length qt == (2 ^ dim proxy) ^ l

    prop "length of toList is correct" $ \(Level l, ext::Extent v srid) ->
      l <= 3 ==>
        let gen = Node (\e -> return (e, gen))
            Right qt = runIdentity (generate2 gen ext (Level l))
        in length (toList qt) == (2 ^ dim proxy) ^ l


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

    prop "can lookup point inside in a leaf" $
      forAll qtAndPointsInside $ \(qt, p :: Point v NoCrs, _) ->
        case lookupByPoint qt p of
          Nothing  -> False
          Just e   -> e `contains` p

    prop "can lookup any point" $
      forAll ((,) <$> randomQt <*> arbitrary) $ \(qt, p :: Point v NoCrs) ->
        case lookupByPoint qt p of
          Nothing  -> not (qtExtent qt `contains` p)
          Just e   -> e `contains` p

  describe "growToInclude" $ do
    prop "if it grows it includes point" $
      forAll qtAndPointsOutsideButNear $ \(qt, p :: Point v NoCrs, _) ->
        let eQt       = runIdentity (growToInclude build p qt)
            build     = Node (\e -> return (e, build))
            Right qt2 = eQt
        in isRight eQt ==> qt2 `qtContainsPoint` p

  when (dim proxy <= 4) $
    describe "traceRay" $ do
      describe "points inside tree" $
        traceRaySpec proxy qtAndPointsInside
      describe "points very close to edges" $
        traceRaySpec proxy qtAndPointsCloseToEdges
      describe "points on edges" $
        traceRaySpec proxy qtAndPointsOnEdges
      describe "points inside after growing" $ do
        let gen = do
              (qt, p, _) <- qtAndPointsOutsideButNear
              eQt <- growToInclude (randomBuild 0.5) p qt
              case eQt of
                Right qt2 -> do
                  p1 <- oneof [ pointOnEdges qt2
                              , pointCloseToEdges qt2]
                  return (qt2, p, p1)
                Left _ -> gen
        traceRaySpec proxy gen



traceRaySpec
  :: forall v.
  ( HasHyperplanes v
  , Show (v Half)
  , Show (v Int)
  , Num (v Int)
  , Eq (v Int)
  , Show (v NeighborDir)
  , Show (HyperPlaneDirections v)
  )
  => Proxy v
  -> Gen (QtAndPoints v)
  -> Spec
traceRaySpec _ generator = do
  prop "does not get stuck, fail assertions or throw errors" $
    forAll generator $ \(qt, p, p1) ->
      let notSameExt a b
            | a == b    = traceShow ("repeated", a, b) False
            | otherwise = True
          els = traceRay qt p p1
      in length (take 2 els) > 1 ==>
           all id (zipWith notSameExt els (tail els))

  prop "a ray from A to A belonging to Qtree returns a singleton" $
    forAll generator $ \(qt, p, _) ->
      qtContainsPoint qt p  ==>
        case traceRay qt p p of
          []  -> error "did not return any els in test"
          (a:[])
            | Just a == lookupByPoint qt p             -> True
            | traceShow ("a->a",qtExtent qt, a,p) True -> False
          _   -> error "traceRay returns junk"

  prop "last contains to" $
    forAll generator $ \(qt, p, p1) ->
      qtContainsPoint qt p && qtContainsPoint qt p1 ==>
        case traceRay qt p p1 of
          []  -> error "did not return any els in test"
          els | Just (last els) == lookupByPoint qt p1  -> True
              | traceShow ("last",qtExtent qt,last els,p1) True -> False
          _   -> error "traceRay returns junk"

  prop "if from and to are in qtree then head contains from" $
    forAll generator $ \(qt, p, p1) ->
      qtContainsPoint qt p && qtContainsPoint qt p1 ==>
        case traceRay qt p p1 of
          []  -> error "did not return any els in property test"
          els -> Just (head els) == lookupByPoint qt p
