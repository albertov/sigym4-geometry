{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (replicateM)
import System.IO hiding (hGetContents)
import System.Random (setStdGen, mkStdGen)
import Data.ByteString.Lazy (ByteString, hGetContents)

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.Binary
import Sigym4.Geometry.QuadTree
import Sigym4.Geometry.QuadTree.Arbitrary (RandomQT(..), randomQtOfLevel)

import Criterion.Main
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  setRandomSeed
  defaultMain [
    bgroup "binary" [
        env loadBigGeomData $
          bench "wkbDecode" .
            nf (wkbDecode :: ByteString -> Either String (Geometry V2 0))
      , env loadBigGeom $
          bench "wkbEncode LittleEndian" . nf (wkbEncode LittleEndian)
      , env loadBigGeom $
          bench "wkbEncode BigEndian" . nf (wkbEncode BigEndian)
      ]
    , bgroup "algorithms" [
        env loadBigGeom $
          bench "extent" . nf extent
      ]
    , bgroup "quadtree" [
        env (randomTrees 100 maxBound) $
          bench "traceRay V2" . nf
            (map (\(RandomQT (qt,p,p1) :: RandomQT V2) -> traceRay qt p p1))
      , env (randomTrees 100 5) $
          bench "traceRay V3" . nf
            (map (\(RandomQT (qt,p,p1) :: RandomQT V3) -> traceRay qt p p1))
      , env (randomTrees 100 3) $
          bench "traceRay V4" . nf
            (map (\(RandomQT (qt,p,p1) :: RandomQT V4) -> traceRay qt p p1))
      ]
    ]

setRandomSeed :: IO ()
setRandomSeed = setStdGen (mkStdGen 12345)

loadBigGeomData :: IO ByteString
loadBigGeomData
  = hGetContents =<< openFile "tests/fixtures/big_geom.wkb" ReadMode

loadBigGeom :: IO (Geometry V2 0)
loadBigGeom = do
  bs <- loadBigGeomData
  return (either (error "could not decode big_geom.wkb") id (wkbDecode bs))

randomTrees :: VectorSpace v => Int -> Level -> IO [RandomQT v]
randomTrees nTrees = replicateM nTrees . QC.generate . randomQtOfLevel
