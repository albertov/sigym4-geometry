{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Sigym4.Geometry.Binary (
    ByteOrder (..)
  , wkbEncode
  , wkbDecode
) where

#include "MachDeps.h"

import Control.Monad.Reader
import Control.Lens
import Data.Proxy (Proxy(Proxy))
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import Sigym4.Geometry.Types
import Data.Binary
import Data.Bits
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

wkbEncode :: BinaryBO g => ByteOrder -> g -> ByteString
wkbEncode bo = runPut . flip runReaderT bo . putBO
{-# INLINABLE wkbEncode #-}

wkbDecode :: Binary o => ByteString -> Either String o
wkbDecode s = case decodeOrFail s of
                Left  (_,_,e) -> Left e
                Right (_,_,a) -> Right a
{-# INLINABLE wkbDecode #-}

type Env      = ByteOrder
type PutBO    = ReaderT Env PutM ()
type GetBO  a = ReaderT Env Get a

askBO :: MonadReader Env m => m ByteOrder
askBO = ask


class BinaryBO a where
    putBO :: a -> PutBO
    getBO :: GetBO a


--
-- Instances
--
--
instance (KnownCrs (Coded t s), BinaryBO (WithCrs (Geometry v)))
  => BinaryBO (Tagged (Coded t s) (Geometry v)) where
  putBO = putBO . untagCrs
  getBO = do
    t <- getBO
    withTaggedCrs t $ \(ret :: Tagged crs2 (Geometry v)) ->
      case sameCrs (Proxy :: Proxy (Coded t s)) (Proxy :: Proxy crs2) of
        Just Refl -> return ret
        Nothing   -> fail "Binary(Tagged crs): crs mismatch"

instance (VectorSpace v, KnownCrs (Coded t s), Binary (WithCrs (Geometry v)))
  => Binary (Tagged (Coded t s) (Geometry v)) where
  put = void . flip runReaderT nativeEndian . putBO
  get = runReaderT getBO =<< get

instance VectorSpace v => Binary (WithCrs (Geometry v)) where
    put = void . flip runReaderT nativeEndian . putBO
    get = runReaderT getBO =<< get
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}

instance VectorSpace v => Binary (Geometry v) where
    put = put . WithCrs noCrs
    get = liftM discardCrs get
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}

sridFlag, zFlag, mFlag :: Word32
sridFlag = 0x20000000
zFlag = 0x80000000
mFlag = 0x40000000

geomType :: forall v. VectorSpace v
  => Geometry v -> Word32
geomType g
  = let flags = (if dim (Proxy :: Proxy v) >= 3 then zFlag else 0)
              + (if dim (Proxy :: Proxy v) == 4 then mFlag else 0)
    in flags + case g of
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

discardCrs :: WithCrs t -> t
discardCrs (WithCrs _ o) = o

instance forall v. VectorSpace v => BinaryBO (Geometry v) where
  putBO = putBO . WithCrs noCrs
  getBO = liftM discardCrs getBO

instance forall v. VectorSpace v => BinaryBO (WithCrs (Geometry v)) where
    putBO (WithCrs crs g) = do
        askBO >>= lift . put
        case crs of
          Coded _ srid -> do
            putBO (geomType g + sridFlag)
            putWord32bo $ fromIntegral srid
          NoCrs ->
            putBO (geomType g)
          _ -> fail "Binary(WithCrs Geometry): expected a coded crs"
        case g of
            GeoPoint g' -> putBO g'
            GeoLineString g' ->  putBO g'
            GeoPolygon g' -> putBO g'
            GeoTriangle g' -> putBO g'
            GeoMultiPoint g' -> putBO g'
            GeoMultiLineString g' -> putBO g'
            GeoMultiPolygon g' -> putBO g'
            GeoCollection g' ->  putBO g'
            GeoPolyhedralSurface g' -> putBO g'
            GeoTIN g' -> putBO g'

    getBO = do
        type_ <- getWord32bo
        let testFlag f = type_ .&. f /= 0
        rSrid <- if testFlag sridFlag
                    then fmap (Just . fromIntegral) getWord32bo
                    else return Nothing
        geom <- case type_ .&. 0x000000ff of
           1  -> geoPoint
           2  -> geoLineString
           3  -> geoPolygon
           17 -> geoTriangle
           4  -> geoMultiPoint
           5  -> geoMultiLineString
           6  -> geoMultiPolygon
           7  -> geoCollection
           15 -> geoPolyhedralSurface
           16 -> geoTIN
           _  -> fail "get(Geometry): wrong geometry type"
        crs <- case rSrid of
                 Just srid ->
                   maybe (fail "wkbDecode: invalid srid") return (epsgCrs srid)
                 Nothing -> return noCrs
        return $ WithCrs crs geom
      where
        geoPoint = GeoPoint <$> getBO
        geoLineString = GeoLineString <$> getBO
        geoPolygon = GeoPolygon <$> getBO
        geoTriangle = GeoTriangle <$> getBO
        geoMultiPoint = GeoMultiPoint <$> getBO
        geoMultiLineString = GeoMultiLineString <$> getBO
        geoMultiPolygon = GeoMultiPolygon <$> getBO
        geoCollection = GeoCollection <$> getBO
        geoPolyhedralSurface = GeoPolyhedralSurface <$> getBO
        geoTIN = GeoTIN <$> getBO

unwrapGeo
  :: VectorSpace v
  => String -> (Geometry v -> Maybe a) -> GetBO (V.Vector a)
unwrapGeo msg f =
  justOrFail msg
    =<< fmap (G.sequence . G.map f) (getVector (lift get))
{-# INLINE unwrapGeo #-}

instance forall v. VectorSpace v => BinaryBO (Point v) where
    getBO = Point <$> (justOrFail "getBO(Point v)" . fromCoords
                       =<< replicateM (dim (Proxy :: Proxy v)) getBO)
    putBO = mapM_ putBO . coords . (^.vertex)

instance forall v. VectorSpace v
  => BinaryBO (MultiPoint v) where
    getBO = MultiPoint  . G.convert <$> unwrapGeo "MultiPoint" (^?_GeoPoint)
    putBO = putVectorBo . V.map GeoPoint . V.convert . (^.points)

instance forall v. VectorSpace v
  => BinaryBO (GeometryCollection v) where
    getBO = GeometryCollection  <$> getVector (lift get)
    putBO = putVectorBo . (^.geometries)

instance forall v. VectorSpace v => BinaryBO (LineString v) where
    getBO = justOrFail "getBO(LineString)" . mkLineString =<< getListBo
    putBO = putVectorBo . (^.points)

instance forall v. VectorSpace v
  => BinaryBO (MultiLineString v) where
    getBO = MultiLineString <$> unwrapGeo "MultiLineString" (^?_GeoLineString)
    putBO = putVectorBo . G.map GeoLineString . (^.lineStrings)

instance forall v. VectorSpace v => BinaryBO (LinearRing v) where
    getBO = justOrFail "getBO(LinearRing)" . mkLinearRing =<< getListBo
    putBO = putVectorBo . (^.points)

instance forall v. VectorSpace v => BinaryBO (Polygon v) where
    getBO = justOrFail "getBO(Polygon)" . mkPolygon =<< getListBo
    putBO = putVectorBo . polygonRings

instance forall v. VectorSpace v
  => BinaryBO (MultiPolygon v) where
    getBO = MultiPolygon <$> unwrapGeo "MultiPolygon" (^?_GeoPolygon)
    putBO = putVectorBo . G.map GeoPolygon . (^.polygons)

instance forall v. VectorSpace v => BinaryBO (Triangle v) where
    getBO = do
        nRings <- getWord32bo
        when (nRings/=1) $ fail "getBO(Triangle): expected a single ring"
        nPoints <- getWord32bo
        when (nPoints/=4) $ fail "getBO(Triangle): expected a 4-point ring"
        (a,b,c,a') <- (,,,) <$> getBO <*> getBO <*> getBO <*> getBO
        when (a /= a') $ fail "getBO(Triangle): first point /= last point"
        justOrFail "getBO(Triangle): invalid triangle" $ mkTriangle a b c
    putBO (Triangle a b c) = putWord32bo 1 >> putWord32bo 4
                          >> putBO a >> putBO b >> putBO c >> putBO a

instance forall v. VectorSpace v => BinaryBO (PolyhedralSurface v) where
    getBO = PolyhedralSurface <$> getVector getBO
    putBO = putVectorBo . (^.polygons)

instance forall v. VectorSpace v => BinaryBO (TIN v) where
    getBO = TIN <$> getVector getBO
    putBO = putVectorBo . (^.triangles)

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
getWord32bo = askBO >>= lift . byteOrder getWord32be getWord32le

putWord32bo :: Word32 -> PutBO
putWord32bo w = askBO >>= lift . byteOrder (putWord32be w) (putWord32le w)

getFloat64bo :: GetBO Double
getFloat64bo = askBO >>= lift . byteOrder getFloat64be getFloat64le

putFloat64bo :: Double -> PutBO
putFloat64bo w = askBO >>= lift . byteOrder (putFloat64be w) (putFloat64le w)

byteOrder :: a -> a -> ByteOrder -> a
byteOrder a b = \case {BigEndian->a; LittleEndian->b}
