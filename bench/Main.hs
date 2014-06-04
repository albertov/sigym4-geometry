module Main where

import Criterion.Main
import System.IO hiding (hGetContents)
import Sigym4.Geometry
import Sigym4.Geometry.Binary (ByteOrder(..), wkbEncode, wkbDecode)
import Test.QuickCheck (arbitrary, generate)
import Data.ByteString.Lazy (ByteString, hGetContents)

import Arbitrary ()

main :: IO ()
main = do
  bs <- hGetContents =<< openFile "tests/fixtures/big_geom.wkb" ReadMode
  let decode = wkbDecode
      decode :: ByteString -> Either String (Geometry MultiPolygon V2)
      Right geom = decode bs
  defaultMain [
      bench "decode" $ whnf decode bs
    , bench "encode" $ nf (wkbEncode NDR) geom
    ]

multipolygon :: IO (Geometry MultiPolygon V3)
multipolygon = generate arbitrary
