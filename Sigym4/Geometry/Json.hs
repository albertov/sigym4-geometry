{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , ScopedTypeVariables
           , GADTs
           , OverloadedStrings
           , DataKinds
           #-}
module Sigym4.Geometry.Json (
    jsonEncode
  , jsonDecode
  , jsonDecodeAny
  ) where

import Control.Applicative
import Data.Aeson
import Data.Typeable
import Sigym4.Geometry.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

jsonEncode :: (Typeable t, IsVertex v Double)
           => Geometry t v -> ByteString
jsonEncode = encode

jsonDecode :: (Typeable t, IsVertex v Double)
           => ByteString -> Either String (Geometry t v)
jsonDecode bs = do
    anyGeom <- eitherDecode bs
    maybe (fail "jsonDecode: wrong geometry") return $ fromAnyGeometry anyGeom

jsonDecodeAny :: FromJSON a => ByteString -> Either String a
jsonDecodeAny = eitherDecode

instance (IsVertex v Double, Typeable t)
  => ToJSON (Geometry t v) where
    toJSON p@(MkPoint _)
        = object [ "type"        .= ("Point" :: String)
                 , "coordinates" .= pointCoords p]
    toJSON (MkMultiPoint ps)
        = object [ "type"        .= ("MultiPoint" :: String)
                 , "coordinates" .= (mkMultiCoords pointCoords $ ps)]
    toJSON ls@(MkLineString _)
        = object [ "type"        .= ("LineString" :: String)
                 , "coordinates" .= lineStringCords ls]
    toJSON (MkMultiLineString ls)
        = object [ "type"        .= ("MultiLineString" :: String)
                 , "coordinates" .= (mkMultiCoords lineStringCords $ ls)
                 ]
    toJSON p@(MkPolygon _)
        = object [ "type"        .= ("Polygon" :: String)
                 , "coordinates" .= polygonCoords p
                 ]
    toJSON (MkMultiPolygon ps)
        = object [ "type"        .= ("MultiPolygon" :: String)
                 , "coordinates" .= (mkMultiCoords polygonCoords  $ ps)
                 ]
    toJSON g@(MkGeometry _ tr)
        | tr == typeOf (undefined :: Geometry Point v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry Point v))
        | tr == typeOf (undefined :: Geometry MultiPoint v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry MultiPoint v))
        | tr == typeOf (undefined :: Geometry LineString v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry LineString v))
        | tr == typeOf (undefined :: Geometry MultiLineString v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry MultiLineString v))
        | tr == typeOf (undefined :: Geometry Polygon v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry Polygon v))
        | tr == typeOf (undefined :: Geometry MultiPolygon v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry MultiPolygon v))
        | tr == typeOf (undefined :: Geometry GeometryCollection v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry GeometryCollection v))
        | tr == typeOf (undefined :: Geometry AnyGeometry v)
        = toJSON  (fromAnyGeometry g :: Maybe (Geometry AnyGeometry v))
        | otherwise = error $ "toJSON(MkGeometry): Unsupported geometry: " ++
                              show tr
    toJSON (MkGeometryCollection geoms)
        = object [ "type"       .= ("GeometryCollection" :: String)
                 , "geometries" .= map toJSON (V.toList geoms)
                 ]

pointCoords :: IsVertex v Double => Geometry Point v -> [Double]
pointCoords = coords . _pVertex

lineStringCords :: IsVertex v Double => Geometry LineString v -> [[Double]]
lineStringCords = map coords . U.toList . _lsVertices

polygonCoords :: IsVertex v Double => Geometry Polygon v -> [[[Double]]]
polygonCoords = map (map coords . U.toList) . V.toList . _pRings

mkMultiCoords :: (a -> b) -> V.Vector a -> [b]
mkMultiCoords p = map p . V.toList

instance (IsVertex v Double, Typeable v)
  => FromJSON (Geometry AnyGeometry v) where
    parseJSON (Object v) = do
        typ    <- v .: "type"
        case (typ :: String) of
            "Point" -> do
              vertex <- fromCoords <$> v.: "coordinates"
              maybe (fail "vertex of wrong dimensions")
                    (return . toAnyGeometry . MkPoint)
                    vertex
            "MultiPoint" -> do
              vertices <- mapM fromCoords <$> v.: "coordinates"
              maybe (fail "parseJson(MultiPoint): vertex of wrong dimensions")
                    (return . toAnyGeometry . MkMultiPoint . V.map MkPoint .
                     V.fromList)
                    vertices
            "LineString" -> do
              vertices <- mapM fromCoords <$> v.: "coordinates"
              maybe (fail "parseJson(LineString): vertex of wrong dimensions")
                    (return . toAnyGeometry . MkLineString . U.fromList )
                    vertices
            "MultiLineString" -> do
              linestrings <- mapM (mapM fromCoords) <$> v.: "coordinates"
              maybe (fail "parseJson(MultiLineString): vertex of wrong dimensions")
                    (return . toAnyGeometry . MkMultiLineString
                    . V.map (MkLineString . U.fromList) . V.fromList)
                    linestrings
            "Polygon" -> do
              rings <- mapM (mapM fromCoords) <$> v.: "coordinates"
              maybe (fail "parseJson(Polygon): vertex of wrong dimensions")
                    ( return . toAnyGeometry . MkPolygon . V.map U.fromList
                    . V.fromList)
                    rings
            "MultiPolygon" -> do
              multirings <- mapM (mapM (mapM fromCoords)) <$> v.: "coordinates"
              maybe (fail "parseJson(MultiPolygon): vertex of wrong dimensions")
                    ( return . toAnyGeometry . MkMultiPolygon 
                    . V.map (MkPolygon . V.map U.fromList . V.fromList)
                    . V.fromList)
                    multirings
            "GeometryCollection" ->
              toAnyGeometry . MkGeometryCollection . V.fromList <$>
              v.:"geometries"
                    
            _ -> fail $ "parseJson(Geometry): Unsupported Geometry: " ++ typ
    parseJSON _ = fail "parseJSON(Geometry): Expected an object"

        

instance (ToJSON (Geometry t v), ToJSON d) => ToJSON (Feature t v d) where
    toJSON (Feature g ps) = object [ "type"       .= ("Feature" :: String)
                                   , "geometry"   .= g
                                   , "properties" .= ps
                                   ]
instance (FromJSON (Geometry t v), FromJSON d) => FromJSON (Feature t v d)
  where
    parseJSON (Object o) = do
        typ <- o .:"type"
        if typ == "Feature"
            then Feature <$> o.: "geometry" <*> o.:"properties"
            else fail $ "parseJSON(Feature): type mismatch: " ++ typ
    parseJSON _ = fail "parseJSON(Feature): Expected an object"


instance (ToJSON d, ToJSON (Feature AnyGeometry v d))
  => ToJSON (FeatureCollection v d)
  where
    toJSON fs = object [ "type"       .= ("FeatureCollection" :: String)
                       , "features"   .= fs]

instance FromJSON (Feature AnyGeometry v d) => FromJSON (FeatureCollection v d)
  where
    parseJSON (Object o) = do
        typ <- o .:"type"
        if typ == "FeatureCollection"
            then o.: "features"
            else fail $ "parseJSON(FeatureCollection): type mismatch: " ++ typ
    parseJSON _ = fail "parseJSON(FeatureCollection): Expected an object"
