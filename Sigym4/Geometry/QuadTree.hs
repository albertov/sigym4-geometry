{-# LANGUAGE TemplateHaskell #-}
module Sigym4.Geometry.QuadTree (
    QuadTree
  , Quadrant (..)
  , Half (..)
  , Node (..)
  , Level (unLevel)
  , Neighbor
  , Neighbors
  , NeighborDir (..)

  , generate
  , generate2
  , grow

  , lookupByPoint
  , traceRay

  , qtExtent
  , qtLevel
  , qtMinBox
) where

import Sigym4.Geometry
import Sigym4.Geometry.QuadTree.Internal.Types
import Sigym4.Geometry.QuadTree.Internal.Algorithms



neighborsV2 :: Neighbors V2
neighborsV2 = $$(mkNeighbors)
{-# INLINE neighborsV2 #-}
{-# RULES "neighbors/V2" neighbors = neighborsV2 #-}

neighborsV3 :: Neighbors V3
neighborsV3 = $$(mkNeighbors)
{-# INLINE neighborsV3 #-}
{-# RULES "neighbors/V3" neighbors = neighborsV3 #-}

neighborsV4 :: Neighbors V4
neighborsV4 = $$(mkNeighbors)
{-# INLINE neighborsV4 #-}
{-# RULES "neighbors/V4" neighbors = neighborsV4 #-}
