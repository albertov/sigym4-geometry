{-# LANGUAGE GADTs
           , FlexibleContexts
           #-}
module Sigym4.Geometry.Algorithms (
    extent
) where

import Prelude as P
import Sigym4.Geometry.Types
import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import Data.Vector as V (Vector, length, toList, head)
import Data.Vector.Unboxed as U (toList)
import qualified Data.Semigroup as SG
import Data.List.NonEmpty as NE

extent :: (Typeable t, IsVertex v Double, SG.Semigroup (Extent v))
    => Geometry t v -> Maybe (Extent v)
extent (MkPoint p) = Just $ Extent p p
extent (MkMultiPoint gs) = multiExtent gs
extent (MkLineString ls) = lrExtent ls
extent (MkMultiLineString gs) = multiExtent gs
extent (MkPolygon rings)
    | V.length rings > 0 = lrExtent $ V.head rings
    | otherwise          = Nothing
extent (MkMultiPolygon gs) = multiExtent gs
extent (MkGeometryCollection gs) = multiExtent gs
extent geom
  = case fromAnyGeometry geom of
       Just g@MkPoint{}             -> extent g
       Just g@MkMultiPoint{}        -> extent g
       Just g@MkLineString{}        -> extent g
       Just g@MkMultiLineString{}   -> extent g
       Just g@MkPolygon{}           -> extent g
       Just g@MkMultiPolygon{}      -> extent g
       Just g@MkGeometryCollection{}-> extent g

multiExtent :: (IsVertex v Double, SG.Semigroup (Extent v))
    => V.Vector (Geometry t v) -> Maybe (Extent v)
multiExtent = fmap SG.sconcat
            . (=<<) NE.nonEmpty
            . P.sequence
            . P.map extent
            . V.toList

lrExtent :: (IsVertex v Double, SG.Semigroup (Extent v))
    => LinearRing v -> Maybe (Extent v)
lrExtent = fmap (SG.sconcat . NE.map (fromJust . extent . MkPoint))
         . NE.nonEmpty
         . U.toList
