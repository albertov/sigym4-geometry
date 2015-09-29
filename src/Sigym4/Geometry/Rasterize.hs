{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Sigym4.Geometry.Rasterize (rasterizeFeatures) where

import Control.Lens ((^.), (^?), views)
import Control.Monad (replicateM, join)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Sigym4.Geometry.Types

rasterizeFeatures :: forall a b v srid. (Stm.Storable a, VectorSpace v, R2 v)
  => GeoReference V2 srid -> a -> [b -> a] -> [Feature v srid b]
  -> [St.Vector a]
rasterizeFeatures geoRef nodata projs features = runST $ do
  rasters <- replicateM (length projs) (Stm.replicate size nodata)
  mapM_ (rasterize rasters) features
  mapM St.unsafeFreeze rasters
  where
    size    = grScalarSize geoRef
    featOff :: Feature v srid b -> Maybe (Offset RowMajor)
    featOff = join . fmap (pointOffset geoRef . to2D) . (^?fGeom._GeoPoint)
    to2D = Point . (\v -> V2 (v^._x) (v^._y)) . (^.pVertex)
    rasterize :: [Stm.MVector s a] -> Feature v srid b -> ST s ()
    rasterize rs f
      = case featOff f of
          Nothing         -> return ()
          Just (Offset o) -> mapM_
                             (\(r,p) -> Stm.unsafeWrite r o (views fData p f))
                             (zip rs projs)
{-# INLINEABLE rasterizeFeatures #-}
