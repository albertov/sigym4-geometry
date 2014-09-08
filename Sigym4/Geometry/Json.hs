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

jsonEncode :: (Typeable t, IsVertex v Double)
           => Geometry t v -> ByteString
jsonEncode = encode

jsonDecode :: (Typeable t, Typeable v, IsVertex v Double)
           => ByteString -> Either String (Geometry t v)
jsonDecode bs = do
    anyGeom <- eitherDecode bs
    maybe (fail "wrong geometry") return $ fromAnyGeometry anyGeom

instance (IsVertex v Double, Typeable t) => ToJSON (Geometry t v) where
    toJSON (MkPoint v)
        = object [ "type"        .= ("Point" :: String)
                 , "coordinates" .= coords v]
    toJSON (MkLineString vs)
        = object [ "type"        .= ("LineString" :: String)
                 , "coordinates" .= (map coords . U.toList $ vs)]

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
            "LineString" -> do
              vertices <- mapM fromCoords <$> v.: "coordinates"
              maybe (fail "vertex of wrong dimensions")
                    (return . toAnyGeometry . MkLineString . U.fromList )
                    vertices

