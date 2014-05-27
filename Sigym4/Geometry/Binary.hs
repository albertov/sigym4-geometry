{-# LANGUAGE RecordWildCards
           , FlexibleContexts
           , KindSignatures
           , DataKinds
           , OverloadedStrings
           , ScopedTypeVariables
           , GADTs
           #-}
module Sigym4.Geometry.Binary (ByteOrder(..), wkbEncode, wkbDecode) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.ByteString.Lazy (ByteString)

import Data.Proxy (Proxy(..))
import Data.Binary (Binary(..))
import Data.Binary.IEEE754 ( getFloat64le, putFloat64le, getFloat64be
                           , putFloat64be)
import Data.Binary.Get (Get, runGetOrFail, getWord8, isEmpty, getWord32le, getWord32be)
import Data.Binary.Put (Put, runPut, putWord8, putWord32le, putWord32be)

import Sigym4.Geometry.Types


wkbEncode :: ByteOrder -> Geometry t v -> ByteString
wkbEncode bo g = runPut (put bo >> putGeometry bo g)

putGeometry :: ByteOrder -> Geometry t v -> Put
putGeometry bo geom
  = case geom of
      MkPoint v -> putGeomType bo Point >> putVertex bo v

putGeomType :: ByteOrder -> GeometryType -> Put
putGeomType bo = putIntBo bo . geomTypeToInt

geomTypeToInt Point = 1
intToGeomType 1 = Point
intToGeomType _ = error "unknown geometry type"

getGeomType = fmap intToGeomType . getIntBo

putVertex :: IsVertex v Double => ByteOrder -> v Double -> Put
putVertex bo = mapM_ (putDoubleBo bo) . vertices

getVertex :: forall v. IsVertex v Double => ByteOrder -> Get (v Double)
getVertex bo = fromVertices =<<
               replicateM (card (Proxy :: Proxy (v Double))) (getDoubleBo bo)

putIntBo    XDR = putWord32be  . fromIntegral
putIntBo    NDR = putWord32le  . fromIntegral

getIntBo    XDR = fmap fromIntegral getWord32be
getIntBo    NDR = fmap fromIntegral getWord32le

putDoubleBo XDR = putFloat64be
putDoubleBo NDR = putFloat64le

getDoubleBo XDR = getFloat64be
getDoubleBo NDR = getFloat64le

class Decodable (t::GeometryType) where
  decodeBody :: IsVertex v Double =>
                ByteOrder -> ByteString -> Either String (Geometry t v)
  dtype      :: Proxy t -> GeometryType

instance Decodable 'Point where
  dtype _ = Point
  decodeBody bo = runGet' (MkPoint  <$> getVertex bo)

wkbDecode :: forall t v. (Decodable t, IsVertex v Double)
  => ByteString -> Either String (Geometry t v)
wkbDecode bs
  = case runGetOrFail getHeader bs of
     Right (rest,_,(bo,geomtype))
       | geomtype == dtype (Proxy :: Proxy t) -> decodeBody bo rest
       | otherwise -> Left "wkbDecode: unexpected geometry type"
  where
    getHeader = do
      bo <- get
      geomtype <- getGeomType bo
      return $! (bo,geomtype)

runGet' g bs
  = case runGetOrFail g bs of
      Left  (_,_,err) -> Left err
      Right ("",_,v)  -> Right v
      Right (_,_,v)   -> Left "wkbDecode: unconsumed input"
                  
data ByteOrder = XDR | NDR deriving (Show)

instance Binary ByteOrder where
  put XDR = putWord8 0
  put NDR = putWord8 1
  get = fmap (\v -> if v==0 then XDR else NDR) getWord8








 

