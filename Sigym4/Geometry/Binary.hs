{-# LANGUAGE CPP, LambdaCase, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sigym4.Geometry.Binary (
    ByteOrder (..)
  , wkbEncode
  , wkbDecode
) where

#include "MachDeps.h"

import Control.Applicative
import Control.Monad.Reader
import Data.Proxy (Proxy(Proxy))
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector.Generic as G
import Sigym4.Geometry.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754 ( getFloat64le, putFloat64le, getFloat64be
                           , putFloat64be)

data ByteOrder = BigEndian | LittleEndian deriving (Eq, Show, Enum)

nativeEndian :: ByteOrder
#ifdef WORDS_BIGENDIAN
nativeEndian = BigEndian
#else
nativeEndian = LittleEndian
#endif

wkbEncode :: VectorSpace v => ByteOrder -> Geometry v -> ByteString
wkbEncode bo = runPut . flip runReaderT bo . putBO
{-# INLINEABLE wkbEncode #-}

wkbDecode :: VectorSpace v => ByteString -> Either String (Geometry v)
wkbDecode s = case decodeOrFail s of
                Left  (_,_,e) -> Left e
                Right (_,_,a) -> Right a
{-# INLINEABLE wkbDecode #-}

type PutBO = ReaderT ByteOrder PutM ()
type GetBO a = ReaderT ByteOrder Get a

class BinaryBO a where
    putBO :: a -> PutBO
    getBO :: GetBO a


--
-- Instances
--
--
instance VectorSpace v => Binary (Geometry v) where
    put = flip runReaderT nativeEndian . putBO
    get = get >>= runReaderT getBO
    {-# INLINEABLE put #-}
    {-# INLINEABLE get #-}


geomType :: forall v. VectorSpace v => Geometry v -> Word32
geomType g
  = let summand = if dim (Proxy :: Proxy v) == 3 then 1000 else 0
    in summand + case g of
                    GeoPoint _ -> 1
                    GeoLineString _ -> 2
                    GeoPolygon _ -> 3
                    GeoTriangle _ -> 17
                    GeoMultiPoint _ -> 4
                    GeoMultiLineString _ ->  5
                    GeoMultiPolygon _ -> 6
                    GeoCollection _ -> 7
                    GeoPolyhedralSurface _ -> 15
                    GeoTIN _ -> 16

instance forall v. VectorSpace v => BinaryBO (Geometry v) where
    putBO g = do
        ask >>= lift . put
        putBO $ geomType g
        case g of
            GeoPoint g' -> putBO g'
            GeoLineString g' ->  putBO g'
            GeoPolygon g' -> putBO g'
            GeoTriangle g' -> putBO g'
            GeoMultiPoint g' -> putVectorBo (G.map GeoPoint g')
            GeoMultiLineString g' -> putVectorBo (G.map GeoLineString g')
            GeoMultiPolygon g' -> putVectorBo (G.map GeoPolygon g')
            GeoCollection g' ->  putVectorBo g'
            GeoPolyhedralSurface g' -> putBO g'
            GeoTIN g' -> putBO g'

    getBO = getWord32bo
        >>= \t -> case (t,dim (Proxy::Proxy v)) of
                    (1,2)    -> geoPoint
                    (2,2)    -> geoLineString
                    (3,2)    -> geoPolygon
                    (17,2)   -> geoTriangle
                    (4,2)    -> geoMultiPoint
                    (5,2)    -> geoMultiLineString
                    (6,2)    -> geoMultiPolygon
                    (7,2)    -> geoCollection
                    (15,2)   -> geoPolyhedralSurface
                    (16,2)   -> geoTIN
                    (1001,3) -> geoPoint
                    (1002,3) -> geoLineString
                    (1003,3) -> geoPolygon
                    (1017,3) -> geoTriangle
                    (1004,3) -> geoMultiPoint
                    (1005,3) -> geoMultiLineString
                    (1006,3) -> geoMultiPolygon
                    (1007,3) -> geoCollection
                    (1015,3) -> geoPolyhedralSurface
                    (1016,3) -> geoTIN
                    _        -> fail "get(Geometry): wrong dimension"
      where
        unPoint (GeoPoint g) = Just g
        unPoint _            = Nothing
        unLineString (GeoLineString g) = Just g
        unLineString _            = Nothing
        unPolygon (GeoPolygon g) = Just g
        unPolygon _            = Nothing
        geoPoint = GeoPoint <$> getBO
        geoLineString = GeoLineString <$> getBO
        geoPolygon = GeoPolygon <$> getBO
        geoTriangle = GeoTriangle <$> getBO
        geoMultiPoint = GeoMultiPoint <$> unwrapGeo "geoMultiPoint" unPoint
        geoMultiLineString = GeoMultiLineString
                         <$> unwrapGeo "geoMultiLineString" unLineString
        geoMultiPolygon = GeoMultiPolygon
                      <$> unwrapGeo "geoMultiPolygon" unPolygon
        geoCollection = GeoCollection <$> getVector (lift  get)
        geoPolyhedralSurface = GeoPolyhedralSurface <$> getBO
        geoTIN = GeoTIN <$> getBO
        unwrapGeo msg f = justOrFail msg
                      =<< fmap (G.sequence . G.map f) (getVector (lift get))

instance forall v. VectorSpace v => BinaryBO (Point v) where
    getBO = Point <$> (justOrFail "getBO(Point v)" . fromList
                       =<< replicateM (dim (Proxy :: Proxy v)) getBO)
    putBO = mapM_ putBO . toList . _pVertex

instance forall v. VectorSpace v => BinaryBO (LineString v) where
    getBO = justOrFail "getBO(LineString)" . mkLineString =<< getListBo
    putBO = putVectorBo . _lsPoints

instance forall v. VectorSpace v => BinaryBO (LinearRing v) where
    getBO = justOrFail "getBO(LinearRing)" . mkLinearRing =<< getListBo
    putBO = putVectorBo . _lrPoints

instance forall v. VectorSpace v => BinaryBO (Polygon v) where
    getBO = justOrFail "getBO(Polygon)" . mkPolygon =<< getListBo
    putBO = putVectorBo . polygonRings

instance forall v. VectorSpace v => BinaryBO (Triangle v) where
    getBO = do
        nRings <- getBO :: GetBO (Word32)
        when (nRings/=1) $ fail "getBO(Triangle): expected a single ring"
        nPoints <- getBO :: GetBO (Word32)
        when (nPoints/=4) $ fail "getBO(Triangle): expected a 4-point ring"
        (a,b,c,a') <- (,,,) <$> getBO <*> getBO <*> getBO <*> getBO
        when (a /= a') $
            fail "getBO(Triangle): first point /= last point"
        justOrFail "getBO(Triangle): invalid triangle" $ mkTriangle a b c
    putBO (Triangle a b c) = putBO (1 :: Word32)
                          >> putBO (4 :: Word32)
                          >> putBO a
                          >> putBO b
                          >> putBO c
                          >> putBO a

instance forall v. VectorSpace v => BinaryBO (PolyhedralSurface v) where
    getBO = PolyhedralSurface <$> getVector getBO
    putBO = putVectorBo . _psPolygons

instance forall v. VectorSpace v => BinaryBO (TIN v) where
    getBO = TIN <$> getVector getBO
    putBO = putVectorBo . _tinTriangles

justOrFail :: Monad m => String -> Maybe a -> m a
justOrFail msg = maybe (fail msg) return


getVector :: (BinaryBO a, G.Vector v a) => GetBO a -> GetBO (v a)
getVector f  = getWord32bo >>= flip G.replicateM f . fromIntegral

getListBo :: BinaryBO a => GetBO [a]
getListBo = getWord32bo >>= flip replicateM getBO . fromIntegral

putVector :: G.Vector v a => (a -> PutBO) -> v a -> PutBO
putVector p v = putWord32bo (fromIntegral $ G.length v) >> G.mapM_ p v

putVectorBo :: (BinaryBO a, G.Vector v a) => v a -> PutBO
putVectorBo = putVector putBO

instance BinaryBO Double where
    getBO = getFloat64bo
    putBO = putFloat64bo

instance BinaryBO Word32 where
    getBO = getWord32bo
    putBO = putWord32bo

instance Binary ByteOrder where
  put = putWord8 . fromIntegral . fromEnum
  get = fmap (toEnum . fromIntegral) getWord8

getWord32bo :: GetBO Word32
getWord32bo = ask
          >>= lift . \case {BigEndian -> getWord32be; _ -> getWord32le}

putWord32bo :: Word32 -> PutBO
putWord32bo w = ask
          >>= lift . \case {BigEndian -> putWord32be w; _ -> putWord32le w}

getFloat64bo :: GetBO Double
getFloat64bo = ask
          >>= lift . \case {BigEndian -> getFloat64be; _ -> getFloat64le}

putFloat64bo :: Double -> PutBO
putFloat64bo w = ask
          >>= lift . \case {BigEndian -> putFloat64be w; _ -> putFloat64le w}
