{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , CPP
           , ScopedTypeVariables
           , KindSignatures
           , DataKinds
           , OverloadedStrings
           , DefaultSignatures
           , ExistentialQuantification
           , RankNTypes
           #-}
module Sigym4.Geometry.Json (
    encode
  , eitherDecode
  , FromFeatureProperties (..)
  , ToFeatureProperties (..)
  , SomeFeatureT (..)
  , SomeFeatureCollectionT (..)
  , SomeGeometry (..)
  , HasSameCrs (..)
  ) where

import Data.Aeson
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, unpack)
import Data.Aeson.Types (Parser, Pair, parseMaybe)
import Sigym4.Geometry.Types
import Control.Lens ((^.))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as HM
import GHC.TypeLits


instance (KnownSymbol crs, VectorSpace v) => ToJSON (Point v crs) where
    toJSON g = typedObject "Point" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (MultiPoint v crs) where
    toJSON g = typedObject "MultiPoint" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (LineString v crs) where
    toJSON g = typedObject "LineString" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (MultiLineString v crs) where
    toJSON g = typedObject "MultiLineString" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (Polygon v crs) where
    toJSON g = typedObject "Polygon" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (MultiPolygon v crs) where
    toJSON g = typedObject "MultiPolygon" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (Triangle v crs) where
    toJSON g = typedObject "Triangle" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (PolyhedralSurface v crs) where
    toJSON g = typedObject "PolyhedralSurface" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v) => ToJSON (TIN v crs) where
    toJSON g = typedObject "TIN" ["coordinates" .= coordinates g]

instance (KnownSymbol crs, VectorSpace v)
  => ToJSON (GeometryCollection v crs) where
    toJSON g =
      typedObject "GeometryCollection" ["geometries" .= (g^.geometries)]


instance (KnownSymbol crs, VectorSpace v) => ToJSON (Geometry v crs) where
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

instance (KnownSymbol crs, VectorSpace v) => FromJSON (Point v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parsePoint
    parseJSON _ = fail "parseJSON(Point): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (MultiPoint v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>=
        fmap (MultiPoint . U.convert) . V.mapM parsePoint . V.fromList

    parseJSON _ = fail "parseJSON(MultiPoint): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (LineString v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parseLineString
    parseJSON _ = fail "parseJSON(LineString): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (MultiLineString v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap MultiLineString . V.mapM parseLineString
    parseJSON _ = fail "parseJSON(MultiLineString): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (Polygon v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parsePolygon
    parseJSON _ = fail "parseJSON(Polygon): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (MultiPolygon v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap MultiPolygon . V.mapM parsePolygon
    parseJSON _ = fail "parseJSON(MultiPolygon): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (Triangle v crs) where
    parseJSON (Object o) = parseCoordinates o >>= parseTriangle
    parseJSON _ = fail "parseJSON(Triangle): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (PolyhedralSurface v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap PolyhedralSurface . V.mapM parsePolygon
    parseJSON _ = fail "parseJSON(PolyhedralSurface): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (TIN v crs) where
    parseJSON (Object o) =
      parseCoordinates o >>= fmap (TIN . U.convert) . V.mapM parseTriangle
    parseJSON _ = fail "parseJSON(TIN): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (GeometryCollection v crs) where
    parseJSON (Object o) = fmap GeometryCollection (o .: "geometries")
    parseJSON _ = fail "parseJSON(GeometryCollection): Expected an object"

instance (KnownSymbol crs, VectorSpace v) => FromJSON (Geometry v crs) where
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


class FromFeatureProperties o where
  fromFeatureProperties :: Object -> Parser o
  default fromFeatureProperties :: FromJSON o => Object -> Parser o
  fromFeatureProperties o =
    case parseMaybe parseJSON (Object o) of
      Just o'  -> return o'
      Nothing -> o .: "value"

instance {-# OVERLAPPABLE #-} FromFeatureProperties o
  => FromFeatureProperties (Maybe o) where
    fromFeatureProperties o
      | HM.null o = return Nothing
      | otherwise = fmap Just (fromFeatureProperties o)

instance (ToJSON (g v crs), ToFeatureProperties d)
  => ToJSON (FeatureT g v crs d) where
    toJSON (Feature g ps) =
      typedObject "Feature" [ "geometry"   .= g
                            , "properties" .= toFeatureProperties ps ]

instance {-# OVERLAPPABLE #-} ToFeatureProperties o
  => ToFeatureProperties (Maybe o) where
    toFeatureProperties = maybe HM.empty toFeatureProperties

instance (FromJSON (g v crs), FromFeatureProperties d)
  => FromJSON (FeatureT g v crs d)
  where
    parseJSON (Object o) = do
      props <- case HM.lookup "properties" o of
        Just (Object p) -> fromFeatureProperties p
        Just Null       -> fromFeatureProperties HM.empty
        Nothing         -> fromFeatureProperties HM.empty
        Just _          -> fail "properties field is not an object"
      Feature <$> o .: "geometry" <*> pure props
    parseJSON _ = fail "parseJSON(Feature): Expected an object"


instance (KnownSymbol crs, ToFeatureProperties d, ToJSON (g v crs))
  => ToJSON (FeatureCollectionT g v crs d)
  where
    toJSON f =
      toJSON_crs (Proxy :: Proxy crs) $
        typedObject "FeatureCollection" ["features" .= (f^.features)]

instance (FromFeatureProperties d, FromJSON (g v crs))
  => FromJSON (FeatureCollectionT g v crs d)
  where
    parseJSON (Object o) = FeatureCollection <$> o.: "features"
    parseJSON _ = fail "parseJSON(FeatureCollection): Expected an object"





data SomeGeometry (g :: (* -> *) -> Symbol -> *) v
  = forall crs. (ToJSON (g v crs), KnownSymbol crs) => SomeGeometry (g v crs)

data SomeFeatureT g v a
  = forall crs. (ToJSON (FeatureT g v crs a), KnownSymbol crs)
  => SomeFeatureT (FeatureT g v crs a)

data SomeFeatureCollectionT g v a
  = forall crs. (ToJSON (FeatureCollectionT g v crs a), KnownSymbol crs)
  => SomeFeatureCollectionT (FeatureCollectionT g v crs a)

class HasSameCrs o where
  sameCrs :: o -> o -> Bool

instance HasSameCrs (SomeGeometry g v) where
  sameCrs (SomeGeometry (_ :: g v c1))
          (SomeGeometry (_ :: g v c2)) =
    case sameSymbol (Proxy :: Proxy c1) (Proxy :: Proxy c2) of
      Just _  -> True
      Nothing -> False

instance HasSameCrs (SomeFeatureT g v a) where
  sameCrs (SomeFeatureT (_ :: FeatureT g v c1 a))
          (SomeFeatureT (_ :: FeatureT g v c2 a)) =
    case sameSymbol (Proxy :: Proxy c1) (Proxy :: Proxy c2) of
      Just _  -> True
      Nothing -> False

instance HasSameCrs (SomeFeatureCollectionT g v a) where
  sameCrs (SomeFeatureCollectionT (_ :: FeatureCollectionT g v c1 a))
          (SomeFeatureCollectionT (_ :: FeatureCollectionT g v c2 a)) =
    case sameSymbol (Proxy :: Proxy c1) (Proxy :: Proxy c2) of
      Just _  -> True
      Nothing -> False

instance ToJSON (SomeGeometry g v) where
  toJSON (SomeGeometry (g :: g v crs)) =
    toJSON_crs (Proxy :: Proxy crs) g

instance ToJSON (SomeFeatureT g v a) where
  toJSON (SomeFeatureT (g :: FeatureT g v crs a)) =
    toJSON_crs (Proxy :: Proxy crs) g

instance ToJSON (SomeFeatureCollectionT g v a) where
  toJSON (SomeFeatureCollectionT (g :: FeatureCollectionT g v crs a)) =
    toJSON_crs (Proxy :: Proxy crs) g


toJSON_crs
  :: forall o s. (KnownSymbol s, ToJSON o) => Proxy s -> o -> Value
toJSON_crs p o =
  case (c, toJSON o) of
    ("",z)         -> z
    (_,Object hm) -> Object (HM.insert "crs" crsObject hm)
    (_,z)         -> z
  where
    c = crs p
    crsObject =
      object [ "type"       .= ("name" :: Text)
             , "properties" .=  object ["name" .= c]]


withParsedCrs
  :: Value -> (forall crs. KnownSymbol crs => Proxy crs -> Parser a)
  -> Parser a
withParsedCrs (Object o) f = do
  mCrs <- o .:? "crs"
  case mCrs of
    Just (Object o') -> do
      mProperties <- o' .:? "properties"
      case mProperties of
        Just (Object p) -> do
          mName <- p .: "name"
          case mName of
            Just c -> withCrs c f
            _ -> withCrs "" f
        _ -> withCrs "" f
    _ -> withCrs "" f
withParsedCrs _ f = withCrs "" f


#define fromJSONAnyCrs(ty) \
instance VectorSpace v => FromJSON (SomeGeometry ty v) where { \
  parseJSON o =\
    withParsedCrs o $ \(Proxy :: Proxy crs) -> do {\
;     g :: ty v crs <- parseJSON o\
;     return (SomeGeometry g)}};\
instance (ToFeatureProperties a, FromFeatureProperties a, VectorSpace v) => FromJSON (SomeFeatureT ty v a) where { \
  parseJSON o =\
    withParsedCrs o $ \(Proxy :: Proxy crs) -> do {\
;     f :: FeatureT ty v crs a <- parseJSON o\
;     return (SomeFeatureT f)}};\
instance (ToFeatureProperties a, FromFeatureProperties a, VectorSpace v) => FromJSON (SomeFeatureCollectionT ty v a) where { \
  parseJSON o =\
    withParsedCrs o $ \(Proxy :: Proxy crs) -> do {\
;     f :: FeatureCollectionT ty v crs a <- parseJSON o\
;     return (SomeFeatureCollectionT f)}}

fromJSONAnyCrs(Geometry)
fromJSONAnyCrs(GeometryCollection)
fromJSONAnyCrs(PolyhedralSurface)
fromJSONAnyCrs(TIN)
fromJSONAnyCrs(Triangle)
fromJSONAnyCrs(MultiPolygon)
fromJSONAnyCrs(Polygon)
fromJSONAnyCrs(MultiLineString)
fromJSONAnyCrs(LineString)
fromJSONAnyCrs(MultiPoint)
fromJSONAnyCrs(Point)
