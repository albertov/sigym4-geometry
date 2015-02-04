{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , ScopedTypeVariables
           , OverloadedStrings
           #-}
module Sigym4.Geometry.Json (
    jsonEncode
  , jsonDecode
  ) where

import Control.Applicative
import Data.Aeson
import Data.Text (Text, unpack)
import Data.Aeson.Types (Parser, Pair)
import Sigym4.Geometry.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

jsonEncode :: (VectorSpace v)
           => Geometry v srid -> ByteString
jsonEncode = encode

jsonDecode :: (VectorSpace v)
           => ByteString -> Either String (Geometry v srid)
jsonDecode = eitherDecode

instance VectorSpace v => ToJSON (Geometry v srid) where
    toJSON (GeoPoint g)
      = typedObject "Point"
        ["coordinates" .= pointCoordinates g]
    toJSON (GeoMultiPoint (MultiPoint g))
      = typedObject "MultiPoint"
        ["coordinates" .= V.map pointCoordinates g]
    toJSON (GeoLineString g)
      = typedObject "LineString"
        ["coordinates" .= lineStringCoordinates g]
    toJSON (GeoMultiLineString g)
      = typedObject "MultiLineString"
        ["coordinates" .= V.map lineStringCoordinates g]
    toJSON (GeoPolygon g)
      = typedObject "Polygon"
        ["coordinates" .= polygonCoordinates g]
    toJSON (GeoMultiPolygon (MultiPolygon g))
      = typedObject "MultiPolygon"
        ["coordinates" .= V.map polygonCoordinates g]
    -- GeoJson spec does not define Triangle, TIN or PolyhedralSurface but we
    -- define them similarily to linering and multipolygons
    toJSON (GeoTriangle g)
      = typedObject "Triangle"
        ["coordinates" .= triangleCoordinates g]
    toJSON (GeoPolyhedralSurface (PolyhedralSurface g))
      = typedObject "PolyhedralSurface"
        ["coordinates" .= V.map polygonCoordinates g]
    toJSON (GeoTIN (TIN g))
      = typedObject "TIN"
        ["coordinates" .= V.map triangleCoordinates (V.convert g)]
    toJSON (GeoCollection g)
      = typedObject "GeometryCollection"
        ["geometries" .= g]
    {-# INLINEABLE toJSON  #-}

parsePoint :: VectorSpace v => [Double] -> Parser (Point v srid)
parsePoint = maybe (fail "parsePoint: wrong dimension") (return . Point) . fromCoords

parsePoints :: VectorSpace v => [[Double]] -> Parser (V.Vector (Point v srid))
parsePoints = V.mapM parsePoint . V.fromList

parseLineString :: VectorSpace v => [[Double]] -> Parser (LineString v srid)
parseLineString ps = do
    points <- mapM parsePoint ps
    maybe (fail "parseLineString: Invalid linestring") return
          (mkLineString points)

parseLinearRing :: VectorSpace v => [[Double]] -> Parser (LinearRing v srid)
parseLinearRing ps = do
    points <- mapM parsePoint ps
    maybe (fail "parseLinearRing: Invalid linear ring") return
          (mkLinearRing points)

parseTriangle :: VectorSpace v => [[Double]] -> Parser (Triangle v srid)
parseTriangle ps = do
    [a,b,c,a'] <- mapM parsePoint ps
    if a/=a' then fail "parseTriangle: last coord must be the same as first"
             else maybe (fail "parseTriangle: invalid triangle")
                        return
                        (mkTriangle a b c)

parsePolygon :: VectorSpace v => [[[Double]]] -> Parser (Polygon v srid)
parsePolygon ps = do
    rings <- V.mapM parseLinearRing (V.fromList ps)
    if V.length rings == 0
       then fail $ "parseJSON(Geometry): Polygon requires at least one linear ring"
       else return $ Polygon (V.head rings) (V.tail rings)

coordinates :: FromJSON a => Object -> Parser a
coordinates o = o .: "coordinates"

typedObject :: Text -> [Pair] -> Value
typedObject k = object . ((:) ("type" .= k))




instance VectorSpace v => FromJSON (Geometry v srid) where
    parseJSON (Object o) = do
        typ <- o .: "type"
        case typ of
            "Point" ->
                coordinates o >>= fmap GeoPoint . parsePoint :: Parser (Geometry v srid)
            "MultiPoint" ->
                coordinates o >>= fmap (GeoMultiPoint . MultiPoint)
                                . parsePoints
            "LineString" ->
                coordinates o >>= fmap GeoLineString . parseLineString
            "MultiLineString" ->
                coordinates o >>= fmap GeoMultiLineString
                                . V.mapM parseLineString
            "Polygon" ->
                coordinates o >>= fmap GeoPolygon . parsePolygon
            "MultiPolygon" ->
                coordinates o >>= fmap (GeoMultiPolygon . MultiPolygon)
                                . V.mapM parsePolygon
            "Triangle" ->
                coordinates o >>= fmap GeoTriangle . parseTriangle
            "PolyhedralSurface" ->
                coordinates o >>= fmap (GeoPolyhedralSurface. PolyhedralSurface)
                                . V.mapM parsePolygon
            "TIN" ->
                coordinates o >>= fmap (GeoTIN . TIN . U.convert)
                           . V.mapM parseTriangle
            "GeometryCollection" ->
                fmap GeoCollection $ o .: "geometries"
            _ -> fail $ "parseJSON(Geometry): Invalid geometry type: " ++ unpack typ
    parseJSON _ = fail "parseJSON(Geometry): Expected an object"
    {-# INLINEABLE parseJSON  #-}

        

instance (ToJSON (Geometry v srid), ToJSON d) => ToJSON (Feature v srid d) where
    toJSON (Feature g ps) = typedObject "Feature" ["geometry"    .= g
                                                  , "properties" .= ps
                                                  ]
instance (FromJSON (Geometry v srid), FromJSON d) => FromJSON (Feature v srid d)
  where
    parseJSON (Object o) = Feature <$> o.: "geometry" <*> o.:"properties"
    parseJSON _ = fail "parseJSON(Feature): Expected an object"


instance (ToJSON d, ToJSON (Geometry v srid))
  => ToJSON (FeatureCollection v srid d)
  where
    toJSON (FeatureCollection fs)
      = typedObject "FeatureCollection" ["features" .= fs]

instance (FromJSON d, FromJSON (Geometry v srid))
  => FromJSON (FeatureCollection v srid d)
  where
    parseJSON (Object o) = do
        typ <- o .:"type"
        if typ == "FeatureCollection"
            then FeatureCollection <$> o.: "features"
            else fail $ "parseJSON(FeatureCollection): type mismatch: " ++ typ
    parseJSON _ = fail "parseJSON(FeatureCollection): Expected an object"
