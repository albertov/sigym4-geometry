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
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry.Types


wkbEncode :: ByteOrder -> Geometry t v -> ByteString
wkbEncode bo g = runPut (put bo >> putGeometry bo g)

putGeometry :: forall v t. ByteOrder -> Geometry t v -> Put
putGeometry bo (MkPoint v)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) Point
 >> putVertex bo v
putGeometry bo (MkLineString vs)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) LineString
 >> putIntBo bo (U.length vs)
 >> U.mapM_ (putVertex bo) vs

putGeomType :: ByteOrder -> Int -> GeometryType -> Put
putGeomType bo card' = putIntBo bo . geomTypeCardToInt card'

geomTypeCardToInt 2 Point = 1
geomTypeCardToInt 3 Point = 1001
geomTypeCardToInt 2 LineString = 2
geomTypeCardToInt 3 LineString = 1002
geomTypeCardToInt _ _ = error "unknown geometry type code"

intToGeomType 1    = Point
intToGeomType 1001 = Point
intToGeomType 2    = LineString
intToGeomType 1002 = LineString
intToGeomType _ = error "unknown geometry type code"

intToGeomCard n | n<1000 = 2
intToGeomCard n | n<2000 = 3
intToGeomCard _ = error "unknown geometry type code"


putVertex :: IsVertex v Double => ByteOrder -> v Double -> Put
putVertex bo = mapM_ (putDoubleBo bo) . coords

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
  geomtype   :: Proxy t -> GeometryType


wkbDecode :: forall t v. (Decodable t, IsVertex v Double)
  => ByteString -> Either String (Geometry t v)
wkbDecode bs
  = case runGetOrFail getHeader bs of
     Right (rest,_,(bo,gtype,gcard))
       | gtype == geomtype (Proxy :: Proxy t)
       , gcard == card (Proxy :: Proxy (v Double)) -> decodeBody bo rest
       | otherwise -> Left "wkbDecode: unexpected geometry type/dims"
  where
    getHeader = do
      bo <- get
      geomCode <- getIntBo bo
      return $! (bo, intToGeomType geomCode, intToGeomCard geomCode)

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

instance Decodable 'Point where
  geomtype _ = Point
  decodeBody bo = runGet' (MkPoint  <$> getVertex bo)


instance Decodable 'LineString where
  geomtype _ = LineString
  decodeBody bo = runGet' $ do
    nPoints <- getIntBo bo
    MkLineString . U.fromList  <$> replicateM nPoints (getVertex bo)


 

