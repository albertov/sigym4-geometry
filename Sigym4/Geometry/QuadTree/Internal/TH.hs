{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sigym4.Geometry.QuadTree.TH (
    mkNeighbors
) where

import Control.Monad (forM)
import Sigym4.Geometry.QuadTree.Internal.Types
import Sigym4.Geometry.Types
import qualified Data.Vector.Generic as G
import Language.Haskell.TH.Syntax


mkNeighbors
  :: forall v. VectorSpace v => Q (TExp (Neighbors v))
mkNeighbors = unsafeTExpCoerce (lift (neighbors :: Neighbors v))

instance Lift (NeighborDir) where
    lift Up   = [| Up |]
    lift Down = [| Down |]
    lift Same = [| Same |]


instance VectorSpace v => Lift (Neighbor v) where
    lift (Ng v) = let l = G.toList (toVector (toVectorN v))
                  in [| Ng (fromVectorN (V (G.fromList l))) |]
