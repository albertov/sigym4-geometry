{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Sigym4.Geometry.Rasterize (rasterizeFeatures) where

import Control.Lens ((^.), views)
import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Sigym4.Geometry.Types

rasterizeFeatures :: forall a b v.
     (Stm.Storable a, IsVertex v Double, R2 v)
  => GeoReference V2 -> a -> [b -> a] -> [Feature Point v b]
  -> [St.Vector a]
rasterizeFeatures geoRef nodata projs features = runST $ do
  rasters <- replicateM (length projs) (Stm.replicate size nodata)
  mapM_ (rasterize rasters) features
  mapM St.unsafeFreeze rasters
  where
    size    = grScalarSize geoRef
    featOff = views (fGeom . pVertex) (vertexOffset geoRef . to2D)
    to2D p  = V2 (p^._x) (p^._y)
    rasterize :: [Stm.MVector s a] -> Feature Point v b -> ST s ()
    rasterize rs f
      = case (featOff f :: Maybe (Offset RowMajor)) of
          Nothing         -> return ()
          Just (Offset o) -> mapM_
                             (\(r,p) -> Stm.unsafeWrite r o (views fData p f))
                             (zip rs projs)
{-# INLINEABLE rasterizeFeatures #-}
