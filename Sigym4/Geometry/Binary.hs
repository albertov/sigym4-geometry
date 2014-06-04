{-# LANGUAGE RecordWildCards
           , FlexibleContexts
           , KindSignatures
           , DataKinds
           , OverloadedStrings
           , ScopedTypeVariables
           , GADTs
           #-}
module Sigym4.Geometry.Binary (
    Decodable
  , ByteOrder(..)
  , wkbEncode
  , wkbDecode
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString.Lazy (ByteString)

import Data.Proxy (Proxy(..))
import Data.Binary (Binary(..))
import Data.Binary.IEEE754 ( getFloat64le, putFloat64le, getFloat64be
                           , putFloat64be)
import Data.Binary.Get (Get, runGetOrFail, getWord8, getWord32le, getWord32be)
import Data.Binary.Put (Put, runPut, putWord8, putWord32le, putWord32be)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V (fromList, length, mapM_)

import Sigym4.Geometry.Types


wkbEncode :: ByteOrder -> Geometry t v -> ByteString
wkbEncode bo g = runPut (put bo >> putGeometry bo g)

putGeometry :: forall v t. ByteOrder -> Geometry t v -> Put
putGeometry bo (MkPoint v)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) Point
 >> putVertex bo v
putGeometry bo (MkMultiPoint points)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) MultiPoint
 >> putIntBo bo (V.length points)
 >> V.mapM_ (\g -> put bo >> putGeometry bo g) points
putGeometry bo (MkLineString vs)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) LineString
 >> putLinearRing bo vs
putGeometry bo (MkMultiLineString lstrings)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) MultiLineString
 >> putIntBo bo (V.length lstrings)
 >> V.mapM_ (\g -> put bo >> putGeometry bo g) lstrings
putGeometry bo (MkPolygon rings)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) Polygon
 >> putIntBo bo (V.length rings)
 >> V.mapM_ (putLinearRing bo) rings
putGeometry bo (MkMultiPolygon polys)
  = putGeomType bo (card (Proxy :: Proxy (v Double))) MultiPolygon
 >> putIntBo bo (V.length polys)
 >> V.mapM_ (\g -> put bo >> putGeometry bo g) polys
{-# INLINE putGeometry #-}

putLinearRing :: IsVertex v Double
  => ByteOrder -> U.Vector (v Double) -> Put
putLinearRing bo vs = putIntBo bo (U.length vs)
                   >> U.mapM_ (putVertex bo) vs
{-# INLINE putLinearRing #-}

putGeomType :: ByteOrder -> Int -> GeometryType -> Put
putGeomType bo card' gtype = putIntBo bo $ geomTypeCardToInt gtype card'
{-# INLINE putGeomType #-}

geomTypeCardToInt :: GeometryType -> Int -> Int
geomTypeCardToInt Point 2 = 1
geomTypeCardToInt Point 3 = 1001
geomTypeCardToInt LineString 2 = 2
geomTypeCardToInt LineString  3 = 1002
geomTypeCardToInt Polygon 2 = 3
geomTypeCardToInt Polygon  3 = 1003
geomTypeCardToInt MultiPoint 2 = 4
geomTypeCardToInt MultiPoint  3 = 1004
geomTypeCardToInt MultiLineString 2 = 5
geomTypeCardToInt MultiLineString  3 = 1005
geomTypeCardToInt MultiPolygon 2 = 6
geomTypeCardToInt MultiPolygon  3 = 1006
geomTypeCardToInt _ _ = error "unknown geometry type code"
{-# INLINE geomTypeCardToInt #-}

intToGeomType :: Int -> GeometryType
intToGeomType 1    = Point
intToGeomType 1001 = Point
intToGeomType 2    = LineString
intToGeomType 1002 = LineString
intToGeomType 3    = Polygon
intToGeomType 1003 = Polygon
intToGeomType 4    = MultiPoint
intToGeomType 1004 = MultiPoint
intToGeomType 5    = MultiLineString
intToGeomType 1005 = MultiLineString
intToGeomType 6    = MultiPolygon
intToGeomType 1006 = MultiPolygon
intToGeomType _ = error "unknown geometry type code"
{-# INLINE intToGeomType #-}

intToGeomCard :: Int -> Int
intToGeomCard n | n<1000 = 2
intToGeomCard n | n<2000 = 3
intToGeomCard _ = error "unknown geometry type code"
{-# INLINE intToGeomCard #-}


putVertex :: IsVertex v Double => ByteOrder -> v Double -> Put
putVertex bo = mapM_ (putDoubleBo bo) . coords
{-# INLINE putVertex #-}

getVertex :: forall v. IsVertex v Double => ByteOrder -> Get (v Double)
getVertex bo = fromVertices =<<
               replicateM (card (Proxy :: Proxy (v Double))) (getDoubleBo bo)
{-# INLINE getVertex #-}

putIntBo :: ByteOrder -> Int -> Put
putIntBo    XDR = putWord32be  . fromIntegral
putIntBo    NDR = putWord32le  . fromIntegral
{-# INLINE putIntBo #-}

getIntBo :: ByteOrder -> Get Int
getIntBo    XDR = fmap fromIntegral getWord32be
getIntBo    NDR = fmap fromIntegral getWord32le
{-# INLINE getIntBo #-}

putDoubleBo :: ByteOrder -> Double -> Put
putDoubleBo XDR = putFloat64be
putDoubleBo NDR = putFloat64le
{-# INLINE putDoubleBo #-}

getDoubleBo :: ByteOrder -> Get Double
getDoubleBo XDR = getFloat64be
getDoubleBo NDR = getFloat64le
{-# INLINE getDoubleBo #-}

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
     Left (_,_,e) -> Left e

getHeader :: Get (ByteOrder, GeometryType, Int)
getHeader = do
  bo <- get
  geomCode <- getIntBo bo
  return $! (bo, intToGeomType geomCode, intToGeomCard geomCode)
{-# INLINE getHeader #-}

runGet' :: Get b -> ByteString -> Either String b
runGet' g bs
  = case runGetOrFail g bs of
      Left  (_,_,err) -> Left err
      Right ("",_,v)  -> Right v
      Right (_,_,_)   -> Left "wkbDecode: unconsumed input"
{-# INLINE runGet' #-}
                  
data ByteOrder = XDR -- ^ Big endian 
               | NDR -- ^ Little endian
  deriving (Show)

instance Binary ByteOrder where
  put XDR = putWord8 0
  put NDR = putWord8 1
  get = fmap (\v -> if v==0 then XDR else NDR) getWord8

instance Decodable 'Point where
  geomtype _ = Point
  decodeBody = runGet' . decodePoint
  {-# INLINE decodeBody #-}

decodePoint :: IsVertex v Double => ByteOrder -> Get (Geometry Point v)
decodePoint = fmap MkPoint . getVertex
{-# INLINE decodePoint #-}

instance Decodable 'MultiPoint where
  geomtype _ = MultiPoint
  decodeBody bo = runGet' $ do
    nPoints <- getIntBo bo
    MkMultiPoint . V.fromList  <$>
      replicateM nPoints (getHeader >>= \(bo',_,_) -> decodePoint bo')
  {-# INLINE decodeBody #-}


instance Decodable 'LineString where
  geomtype _ = LineString
  decodeBody = runGet' . decodeLineString
  {-# INLINE decodeBody #-}

decodeLineString :: IsVertex v Double
  => ByteOrder -> Get (Geometry LineString v)
decodeLineString = fmap MkLineString . decodeLinearRing
{-# INLINE decodeLineString #-}

instance Decodable 'MultiLineString where
  geomtype _ = MultiLineString
  decodeBody bo = runGet' $ do
    nLineStrings <- getIntBo bo
    MkMultiLineString . V.fromList  <$>
      replicateM nLineStrings (getHeader >>= \(bo',_,_) -> decodeLineString bo')
  {-# INLINE decodeBody #-}


decodeLinearRing :: IsVertex v Double => ByteOrder -> Get (LinearRing v)
decodeLinearRing bo = do
  nPoints <- getIntBo bo
  U.fromList  <$> replicateM nPoints (getVertex bo)
{-# INLINE decodeLinearRing #-}

instance Decodable 'Polygon where
  geomtype _ = Polygon
  decodeBody = runGet' . decodePolygon
  {-# INLINE decodeBody #-}

decodePolygon :: IsVertex v Double => ByteOrder -> Get (Geometry Polygon v)
decodePolygon bo = do
    nRings <- getIntBo bo
    MkPolygon . V.fromList  <$> replicateM nRings (decodeLinearRing bo)
{-# INLINE decodePolygon #-}

instance Decodable 'MultiPolygon where
  geomtype _ = MultiPolygon
  decodeBody bo = runGet' $ do
    nPolys <- getIntBo bo
    MkMultiPolygon . V.fromList  <$>
      replicateM nPolys (getHeader >>= \(bo',_,_) -> decodePolygon bo')
  {-# INLINE decodeBody #-}
