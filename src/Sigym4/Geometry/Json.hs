{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , ScopedTypeVariables
           , KindSignatures
           , DataKinds
           , OverloadedStrings
           #-}
module Sigym4.Geometry.Json (
    jsonEncode
  , jsonDecode
  , RootObject (..)
  , FromFeatureProperties (..)
  , ToFeatureProperties (..)
  ) where

import Data.Aeson
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, unpack)
import Data.Aeson.Types (Parser, Pair)
import Sigym4.Geometry.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as HM

jsonEncode
  :: forall o v srid. (VectorSpace v, KnownNat srid, ToJSON (o v srid))
  => o v srid -> ByteString
jsonEncode o = encode (RootObject o)
{-# INLINE jsonEncode #-}

newtype RootObject o = RootObject o

instance (KnownNat s, VectorSpace v, ToJSON (o v s))
  => ToJSON (RootObject (o v s)) where
  toJSON (RootObject o) =
    case toJSON o of
      Object hm -> Object (HM.insert "crs" crsObject hm)
      z         -> z
    where
      crsObject =
        object [
            "type"       .= ("name" :: Text)
          , "properties" .=  object [
              "name" .=
                ("urn:ogc:def:crs:EPSG:" ++ show (gSrid (Proxy::Proxy s)))
            ]
          ]

jsonDecode :: (VectorSpace v)
           => ByteString -> Either String (Geometry v srid)
jsonDecode = eitherDecode
{-# INLINE jsonDecode #-}

instance VectorSpace v => ToJSON (Point v srid) where
    toJSON g = typedObject "Point" ["coordinates" .= pointCoordinates g]

instance VectorSpace v => ToJSON (MultiPoint v srid) where
    toJSON (MultiPoint g) =
      typedObject "MultiPoint" ["coordinates" .= V.map pointCoordinates g]

instance VectorSpace v => ToJSON (LineString v srid) where
    toJSON g =
      typedObject "LineString" ["coordinates" .= lineStringCoordinates g]

instance VectorSpace v => ToJSON (MultiLineString v srid) where
    toJSON (MultiLineString g) =
      typedObject "MultiLineString"
        ["coordinates" .= V.map lineStringCoordinates g]

instance VectorSpace v => ToJSON (Polygon v srid) where
    toJSON g =
      typedObject "Polygon" ["coordinates" .= polygonCoordinates g]

instance VectorSpace v => ToJSON (MultiPolygon v srid) where
    toJSON (MultiPolygon g)
      = typedObject "MultiPolygon"
        ["coordinates" .= V.map polygonCoordinates g]

instance VectorSpace v => ToJSON (Triangle v srid) where
    toJSON g = typedObject "Triangle" ["coordinates" .= triangleCoordinates g]

instance VectorSpace v => ToJSON (PolyhedralSurface v srid) where
    toJSON (PolyhedralSurface g)
      = typedObject "PolyhedralSurface"
        ["coordinates" .= V.map polygonCoordinates g]

instance VectorSpace v => ToJSON (TIN v srid) where
    toJSON (TIN g)
      = typedObject "TIN"
        ["coordinates" .= V.map triangleCoordinates (V.convert g)]

instance (KnownNat srid, VectorSpace v)
  => ToJSON (GeometryCollection v srid) where
    toJSON (GeometryCollection g) =
      typedObject "GeometryCollection" ["geometries" .= g]


instance (KnownNat srid, VectorSpace v) => ToJSON (Geometry v srid) where
    toJSON (GeoPoint g)             = toJSON g
    toJSON (GeoMultiPoint g)        = toJSON g
    toJSON (GeoLineString g)        = toJSON g
    toJSON (GeoMultiLineString g)   = toJSON g
    toJSON (GeoPolygon g)           = toJSON g
    toJSON (GeoMultiPolygon g)      = toJSON g
    -- GeoJson spec does not define Triangle, TIN or PolyhedralSurface but we
    -- define them similarily to linering and multipolygons
    toJSON (GeoTriangle g)          = toJSON g
    toJSON (GeoPolyhedralSurface g) = toJSON g
    toJSON (GeoTIN g)               = toJSON g
    toJSON (GeoCollection g)        = toJSON g
    {-# INLINABLE toJSON  #-}

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

instance VectorSpace v => FromJSON (Point v srid) where
    parseJSON (Object o) = coordinates o >>= parsePoint
    parseJSON _ = fail "parseJSON(Point): Expected an object"

instance VectorSpace v => FromJSON (MultiPoint v srid) where
    parseJSON (Object o) = coordinates o >>= fmap MultiPoint . parsePoints
    parseJSON _ = fail "parseJSON(MultiPoint): Expected an object"

instance VectorSpace v => FromJSON (LineString v srid) where
    parseJSON (Object o) = coordinates o >>= parseLineString
    parseJSON _ = fail "parseJSON(LineString): Expected an object"

instance VectorSpace v => FromJSON (MultiLineString v srid) where
    parseJSON (Object o) =
      coordinates o >>= fmap MultiLineString . V.mapM parseLineString
    parseJSON _ = fail "parseJSON(MultiLineString): Expected an object"

instance VectorSpace v => FromJSON (Polygon v srid) where
    parseJSON (Object o) = coordinates o >>= parsePolygon
    parseJSON _ = fail "parseJSON(Polygon): Expected an object"

instance VectorSpace v => FromJSON (MultiPolygon v srid) where
    parseJSON (Object o) =
      coordinates o >>= fmap MultiPolygon . V.mapM parsePolygon
    parseJSON _ = fail "parseJSON(MultiPolygon): Expected an object"

instance VectorSpace v => FromJSON (Triangle v srid) where
    parseJSON (Object o) = coordinates o >>= parseTriangle
    parseJSON _ = fail "parseJSON(Triangle): Expected an object"

instance VectorSpace v => FromJSON (PolyhedralSurface v srid) where
    parseJSON (Object o) =
      coordinates o >>= fmap PolyhedralSurface . V.mapM parsePolygon
    parseJSON _ = fail "parseJSON(PolyhedralSurface): Expected an object"

instance VectorSpace v => FromJSON (TIN v srid) where
    parseJSON (Object o) =
      coordinates o >>= fmap (TIN . U.convert) . V.mapM parseTriangle
    parseJSON _ = fail "parseJSON(TIN): Expected an object"

instance VectorSpace v => FromJSON (GeometryCollection v srid) where
    parseJSON (Object o) = fmap GeometryCollection (o .: "geometries")
    parseJSON _ = fail "parseJSON(GeometryCollection): Expected an object"

instance VectorSpace v => FromJSON (Geometry v srid) where
    parseJSON v@(Object o) = do
        typ <- o .: "type"
        case typ of
            "Point"              -> parse GeoPoint
            "MultiPoint"         -> parse GeoMultiPoint
            "LineString"         -> parse GeoLineString
            "MultiLineString"    -> parse GeoMultiLineString
            "Polygon"            -> parse GeoPolygon
            "MultiPolygon"       -> parse GeoMultiPolygon
            "Triangle"           -> parse GeoTriangle
            "PolyhedralSurface"  -> parse GeoPolyhedralSurface
            "TIN"                -> parse GeoTIN
            "GeometryCollection" -> parse GeoCollection
            _ -> fail $
              "parseJSON(Geometry): Invalid geometry type: " ++ unpack typ
      where
        parse :: FromJSON a => (a -> b) -> Parser b
        parse c = fmap c (parseJSON v)
    parseJSON _ = fail "parseJSON(Geometry): Expected an object"
    {-# INLINABLE parseJSON  #-}


class ToFeatureProperties o where
  toFeatureProperties :: o -> Object

class FromFeatureProperties o where
  fromFeatureProperties :: Object -> Parser o

instance (ToJSON (Geometry v srid), ToFeatureProperties d)
  => ToJSON (Feature v srid d) where
    toJSON (Feature g ps) =
      typedObject "Feature" [ "geometry"   .= g
                            , "properties" .= toFeatureProperties ps ]

instance (FromJSON (Geometry v srid), FromFeatureProperties d)
  => FromJSON (Feature v srid d)
  where
    parseJSON (Object o) = do
      props <- case HM.lookup "properties" o of
        Just (Object p) -> fromFeatureProperties p
        Just _            -> fail "properties field is not an object"
        Nothing           -> fail "properties key not present"
      Feature <$> o .: "geometry" <*> pure props
    parseJSON _ = fail "parseJSON(Feature): Expected an object"


instance (ToFeatureProperties d, ToJSON (Geometry v srid))
  => ToJSON (FeatureCollection v srid d)
  where
    toJSON (FeatureCollection fs)
      = typedObject "FeatureCollection" ["features" .= fs]

instance (FromFeatureProperties d, FromJSON (Geometry v srid))
  => FromJSON (FeatureCollection v srid d)
  where
    parseJSON (Object o) = do
      FeatureCollection <$> o.: "features"
    parseJSON _ = fail "parseJSON(FeatureCollection): Expected an object"
