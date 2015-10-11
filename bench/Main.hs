{-# LANGUAGE DataKinds #-}
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
  let decode = wkbDecode :: ByteString -> Either String (Geometry V2 0)
      geom   = either (error "could not decode big_geom.wkb") id (decode bs)
  defaultMain [
      bench "decode" $ nf decode bs
    , bench "extent" $ nf (extent :: Geometry V2 0 -> Extent V2 0) geom
    , bench "encode" $ nf (wkbEncode LittleEndian) geom
    ]
