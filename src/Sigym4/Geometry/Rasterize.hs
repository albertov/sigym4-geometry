{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Sigym4.Geometry.Rasterize (rasterizeFeatures) where

import Control.Lens ((^.), (^?), views)
import Control.Monad (replicateM, join)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Sigym4.Geometry.Types

rasterizeFeatures :: forall a b v. (Stm.Storable a, VectorSpace v, R2 v)
  => GeoReference V2 -> a -> [b -> a] -> [Feature v b]
  -> [St.Vector a]
rasterizeFeatures geoRef nodata projs feats = runST $ do
  rasters <- replicateM (length projs) (Stm.replicate size nodata)
  mapM_ (rasterize rasters) feats
  mapM St.unsafeFreeze rasters
  where
    size    = grScalarSize geoRef
    featOff :: Feature v b -> Maybe (Offset RowMajor)
    featOff = join . fmap (pointOffset geoRef . to2D) . (^?geometry._GeoPoint)
    to2D = Point . (\v -> V2 (v^._x) (v^._y)) . (^.vertex)
    rasterize :: [Stm.MVector s a] -> Feature v b -> ST s ()
    rasterize rs f
      = case featOff f of
          Nothing         -> return ()
          Just (Offset o) ->
            mapM_ (\(r,p) -> Stm.unsafeWrite r o (views properties p f))
                  (zip rs projs)
{-# INLINABLE rasterizeFeatures #-}
