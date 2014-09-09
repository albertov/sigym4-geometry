{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GADTs
           , OverloadedStrings
           , DataKinds
           #-}
module Sigym4.Geometry.Json (
    jsonEncode
  , jsonDecode
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
    maybe (fail "wrong geometry") return $ fromAnyGeometry anyGeom

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
    toJSON g = error $ "toJSON(Geometry): Unsupported geometry" ++
                       show (typeOf g)

pointCoords :: IsVertex v Double => Geometry Point v -> [Double]
pointCoords = coords . _pVertex

lineStringCords :: IsVertex v Double => Geometry LineString v -> [[Double]]
lineStringCords = map coords . U.toList . _lsVertices

polygonCoords :: IsVertex v Double => Geometry Polygon v -> [[[Double]]]
polygonCoords = map (map coords . U.toList) . V.toList . _pRings

mkMultiCoords :: (a -> b) -> V.Vector a -> [b]
mkMultiCoords p = map p . V.toList

instance (IsVertex v Double, Typeable v)
  => FromJSON (Geometry 'Geometry v) where
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
            _ -> fail $ "parseJson(Geometry): Unsupported Geometry: " ++ typ
    parseJSON _ = fail "parseJSON(Geometry): Expected an object"

