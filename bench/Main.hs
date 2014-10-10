module Main where

import Criterion.Main
import System.IO hiding (hGetContents)
import Sigym4.Geometry
import Sigym4.Geometry.Algorithms (extent)
import Sigym4.Geometry.Binary (ByteOrder(..), wkbEncode, wkbDecode)
import Data.ByteString.Lazy (ByteString, hGetContents)

main :: IO ()
main = do
  bs <- hGetContents =<< openFile "tests/fixtures/big_geom.wkb" ReadMode
  let decode = wkbDecode
      decode :: ByteString -> Either String (Geometry V2)
      Right geom = decode bs
  defaultMain [
      bench "decode" $ whnf decode bs
    , bench "extent" $ whnf (extent :: Geometry V2 -> Extent V2) geom
    --, bench "encode" $ nf (wkbEncode LittleEndian) geom
    ]
