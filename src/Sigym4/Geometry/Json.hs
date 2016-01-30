{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , ScopedTypeVariables
           , KindSignatures
           , DataKinds
           , OverloadedStrings
           , DefaultSignatures
           , RankNTypes
           , UndecidableInstances
           #-}
module Sigym4.Geometry.Json (
    encode
  , eitherDecode
  , FromFeatureProperties (..)
  , ToFeatureProperties (..)
  ) where

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Aeson.Types (Parser, Pair, parseMaybe)
import Sigym4.Geometry.Types
import Control.Lens ((^.))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as HM


instance VectorSpace v => ToJSON (Point v crs) where
    toJSON g = typedObject "Point" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (MultiPoint v crs) where
    toJSON g = typedObject "MultiPoint" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (LineString v crs) where
    toJSON g = typedObject "LineString" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (MultiLineString v crs) where
    toJSON g = typedObject "MultiLineString" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (Polygon v crs) where
    toJSON g = typedObject "Polygon" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (MultiPolygon v crs) where
    toJSON g = typedObject "MultiPolygon" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (Triangle v crs) where
    toJSON g = typedObject "Triangle" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (PolyhedralSurface v crs) where
    toJSON g = typedObject "PolyhedralSurface" ["coordinates" .= coordinates g]

instance VectorSpace v => ToJSON (TIN v crs) where
    toJSON g = typedObject "TIN" ["coordinates" .= coordinates g]

instance VectorSpace v
  => ToJSON (GeometryCollection v crs) where
    toJSON g =
      typedObject "GeometryCollection" ["geometries" .= (g^.geometries)]


instance VectorSpace v => ToJSON (Geometry v crs) where
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

parsePoint :: VectorSpace v => [Double] -> Parser (Point v crs)
parsePoint =
  maybe (fail "parsePoint: wrong dimension") (return . Point) . fromCoords

parseLineString :: VectorSpace v => [[Double]] -> Parser (LineString v crs)
parseLineString ps = do
    ps' <- mapM parsePoint ps
    maybe (fail "parseLineString: Invalid linestring") return
          (mkLineString ps')

parseLinearRing :: VectorSpace v => [[Double]] -> Parser (LinearRing v crs)
parseLinearRing ps = do
    ps' <- mapM parsePoint ps
    maybe (fail "parseLinearRing: Invalid linear ring") return
          (mkLinearRing ps')

parseTriangle :: VectorSpace v => [[Double]] -> Parser (Triangle v crs)
parseTriangle ps = do
    [a,b,c,a'] <- mapM parsePoint ps
    if a/=a' then fail "parseTriangle: last coord must be the same as first"
             else maybe (fail "parseTriangle: invalid triangle")
                        return
                        (mkTriangle a b c)

parsePolygon :: VectorSpace v => [[[Double]]] -> Parser (Polygon v crs)
parsePolygon ps = do
  rings <- V.mapM parseLinearRing (V.fromList ps)
  if V.length rings == 0
     then fail "parseJSON(Geometry): Polygon requires at least one linear ring"
     else return $ Polygon (V.head rings) (V.tail rings)

parseCoordinates :: FromJSON a => Object -> Parser a
parseCoordinates o = o .: "coordinates"

typedObject :: Text -> [Pair] -> Value
typedObject k = object . ((:) ("type" .= k))

instance VectorSpace v => FromJSON (Point v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parsePoint
    parseJSON _ = fail "parseJSON(Point): Expected an object"

instance VectorSpace v => FromJSON (MultiPoint v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>=
        fmap (MultiPoint . U.convert) . V.mapM parsePoint . V.fromList

    parseJSON _ = fail "parseJSON(MultiPoint): Expected an object"

instance VectorSpace v => FromJSON (LineString v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parseLineString
    parseJSON _ = fail "parseJSON(LineString): Expected an object"

instance VectorSpace v => FromJSON (MultiLineString v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap MultiLineString . V.mapM parseLineString
    parseJSON _ = fail "parseJSON(MultiLineString): Expected an object"

instance VectorSpace v => FromJSON (Polygon v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parsePolygon
    parseJSON _ = fail "parseJSON(Polygon): Expected an object"

instance VectorSpace v => FromJSON (MultiPolygon v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap MultiPolygon . V.mapM parsePolygon
    parseJSON _ = fail "parseJSON(MultiPolygon): Expected an object"

instance VectorSpace v => FromJSON (Triangle v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parseTriangle
    parseJSON _ = fail "parseJSON(Triangle): Expected an object"

instance VectorSpace v => FromJSON (PolyhedralSurface v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap PolyhedralSurface . V.mapM parsePolygon
    parseJSON _ = fail "parseJSON(PolyhedralSurface): Expected an object"

instance VectorSpace v => FromJSON (TIN v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap (TIN . U.convert) . V.mapM parseTriangle
    parseJSON _ = fail "parseJSON(TIN): Expected an object"

instance VectorSpace v => FromJSON (GeometryCollection v crs) where
    parseJSON (Object o) = fmap GeometryCollection (o .: "geometries")
    parseJSON _ = fail "parseJSON(GeometryCollection): Expected an object"

instance VectorSpace v => FromJSON (Geometry v crs) where
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

  default toFeatureProperties :: ToJSON o => o -> Object
  toFeatureProperties o =
    case toJSON o of
      Object o' -> o'
      o'        -> HM.singleton "value" o'

instance {-# OVERLAPPABLE #-} ToJSON o => ToFeatureProperties o


class FromFeatureProperties o where
  fromFeatureProperties :: Object -> Parser o
  default fromFeatureProperties :: FromJSON o => Object -> Parser o
  fromFeatureProperties o =
    case parseMaybe parseJSON (Object o) of
      Just o'  -> return o'
      Nothing -> o .: "value"

instance {-# OVERLAPPABLE #-} FromJSON o => FromFeatureProperties o

instance {-# OVERLAPPABLE #-} FromFeatureProperties o
  => FromFeatureProperties (Maybe o) where
    fromFeatureProperties o
      | HM.null o = return Nothing
      | otherwise = fmap Just (fromFeatureProperties o)

instance {-# OVERLAPPABLE #-} ToFeatureProperties o
  => ToFeatureProperties (Maybe o) where
    toFeatureProperties = maybe HM.empty toFeatureProperties

instance (ToJSON (g crs), ToFeatureProperties d)
  => ToJSON (Feature g d crs) where
    toJSON (Feature g ps) =
      typedObject "Feature" [ "geometry"   .= g
                            , "properties" .= toFeatureProperties ps ]

instance (FromJSON (g crs), FromFeatureProperties d)
  => FromJSON (Feature g d crs)
  where
    parseJSON (Object o) = do
      props <- case HM.lookup "properties" o of
        Just (Object p) -> fromFeatureProperties p
        Just Null       -> fromFeatureProperties HM.empty
        Nothing         -> fromFeatureProperties HM.empty
        Just _          -> fail "properties field is not an object"
      Feature <$> o .: "geometry" <*> pure props
    parseJSON _ = fail "parseJSON(Feature): Expected an object"


instance (ToFeatureProperties d, ToJSON (g crs))
  => ToJSON (FeatureCollection g d crs)
  where
    toJSON f =
        typedObject "FeatureCollection" ["features" .= (f^.features)]

instance (FromFeatureProperties d, FromJSON (g crs))
  => FromJSON (FeatureCollection g d crs)
  where
    parseJSON (Object o) = FeatureCollection <$> o.: "features"
    parseJSON _ = fail "parseJSON(FeatureCollection): Expected an object"
