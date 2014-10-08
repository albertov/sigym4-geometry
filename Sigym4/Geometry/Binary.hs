{-# LANGUAGE LambdaCase
           , ScopedTypeVariables
           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sigym4.Geometry.Binary (
    ByteOrder (..)
  , wkbEncode
  , wkbDecode
) where

import Control.Applicative
import Control.Monad.Reader
import Data.Proxy (Proxy(Proxy))
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector.Generic as V
import Sigym4.Geometry.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754 ( getFloat64le, putFloat64le, getFloat64be
                           , putFloat64be)

data ByteOrder = LittleEndian | BigEndian deriving (Eq, Show)

nativeEndian :: ByteOrder
nativeEndian = LittleEndian -- TODO: use CPP

wkbEncode :: VectorSpace v => ByteOrder -> Geometry v -> ByteString
wkbEncode bo = runPut . flip runReaderT bo . putBO

wkbDecode :: VectorSpace v => ByteString -> Either String (Geometry v)
wkbDecode s = case decodeOrFail s of
                Left  (_,_,e) -> Left e
                Right (_,_,a) -> Right a

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

instance forall v. VectorSpace v => BinaryBO (Geometry v) where
    putBO = undefined
    getBO = getWord32bo
        >>= \t -> case (t,dim (Proxy::Proxy v)) of
                    (1,2) -> GeoPoint <$> getBO
                    (2,2) -> GeoLineString <$> getBO
                    (3,2) -> GeoPolygon <$> getBO
                    (17,2) -> GeoTriangle <$> getBO
                    (4,3) -> GeoMultiPoint <$>
                             getVector (skipHeader >> getBO)
                    (5,3) -> GeoMultiLineString <$>
                             getVector (skipHeader >> getBO)
                    (6,2) -> GeoMultiPolygon <$> getVector (skipHeader >> getBO)
                    (7,2) -> GeoCollection <$> getVectorBo
                    (15,2) -> GeoPolyhedralSurface <$> getBO
                    (16,2) -> GeoTIN <$> getBO

                    (1001,3) -> GeoPoint <$> getBO
                    (1002,3) -> GeoLineString <$> getBO
                    (1003,3) -> GeoPolygon <$> getBO
                    (1017,3) -> GeoTriangle <$> getBO
                    (1004,3) -> GeoMultiPoint <$> getVector (skipHeader >> getBO)
                    (1005,3) -> GeoMultiLineString <$> getVector (skipHeader >> getBO)
                    (1006,3) -> GeoMultiPolygon <$> getVector (skipHeader >> getBO)
                    (1007,3) -> GeoCollection <$> getVectorBo
                    (1015,3) -> GeoPolyhedralSurface <$> getBO
                    (1016,3) -> GeoTIN <$> getBO
                    _        -> fail "get(Geometry): wrong dimension"

skipHeader :: GetBO ()
skipHeader = do _ <- lift get :: GetBO ByteOrder
                _ <- getWord32bo
                return ()

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
    getBO = undefined
    putBO = undefined

instance forall v. VectorSpace v => BinaryBO (PolyhedralSurface v) where
    getBO = undefined
    putBO = undefined

instance forall v. VectorSpace v => BinaryBO (TIN v) where
    getBO = undefined
    putBO = undefined

justOrFail :: Monad m => String -> Maybe a -> m a
justOrFail msg = maybe (fail msg) return


getVector :: (BinaryBO a, V.Vector v a) => GetBO a -> GetBO (v a)
getVector f  = getBO >>= flip V.replicateM f

getVectorBo :: (BinaryBO a, V.Vector v a) => GetBO (v a)
getVectorBo = getVector getBO

getListBo :: BinaryBO a => GetBO [a]
getListBo = getBO >>= flip replicateM getBO


putVectorBo :: (BinaryBO a, V.Vector v a) => v a -> PutBO
putVectorBo v = putBO (V.length v) >> V.mapM_ putBO v

instance BinaryBO Int where
    getBO = fmap fromIntegral getWord32bo
    putBO = putWord32bo . fromIntegral

instance BinaryBO Double where
    getBO = getFloat64bo
    putBO = putFloat64bo

instance Binary ByteOrder where
  put BigEndian = putWord8 0
  put LittleEndian = putWord8 1
  get = fmap (\v -> if v==0 then BigEndian else LittleEndian) getWord8


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
