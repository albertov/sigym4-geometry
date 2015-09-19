{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Sigym4.Geometry.QuadTree (
    QuadTree
  , Quadrant (..)

  , generate
  , grow

  , generateM
  , growM

  , lookupPoint
  , qtExtent

  -- | internal (exposed for testing)
  , innerExtent
  , outerExtent
) where


import Control.Monad (liftM, liftM4)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (runIdentity)

import Sigym4.Geometry (Nat, Extent(..), Point, V2(..))
import Sigym4.Geometry.Algorithms (contains)

data QuadTree (srid :: Nat) a
  = QuadTree {
      qtRoot   :: Node srid a
    , qtExtent :: !(Extent V2 srid)
  } deriving (Eq, Show, Functor)

data Quadrant = NorthWest | NorthEast | SouthWest | SouthEast
  deriving (Show, Eq, Enum, Bounded)


data Node (srid :: Nat) a
  = Leaf a
  | Nil
  | Node { qNW :: Node srid a
         , qNE :: Node srid a
         , qSW :: Node srid a
         , qSE :: Node srid a
         }
  deriving (Eq, Show, Functor)

lookupPoint
  :: QuadTree srid a -> Point V2 srid -> Maybe a
lookupPoint qt p = go (qtRoot qt) (qtExtent qt)
  where
    go Nil _ = Nothing
    go (Leaf v) ext
      | ext `contains` p = Just v
      | otherwise        = Nothing
    go node ext
      | innerExtent NorthWest ext `contains` p
      = go (qNW node) (innerExtent NorthWest ext)
      | innerExtent NorthEast ext `contains` p
      = go (qNE node) (innerExtent NorthEast ext)
      | innerExtent SouthEast ext `contains` p
      = go (qSE node) (innerExtent SouthEast ext)
      | innerExtent SouthWest ext `contains` p
      = go (qSW node) (innerExtent SouthWest ext)
      | otherwise = Nothing


type GenFunc srid a = (Quadrant -> Extent V2 srid -> a -> Maybe a)
type GenFuncM m srid a = (Quadrant -> Extent V2 srid -> a -> m (Maybe a))

generate
  :: Eq a
  => GenFunc srid a
  -> Extent V2 srid -> a -> QuadTree srid a
generate func ext = runIdentity . generateM (\q e -> return . func q e) ext

grow
  :: Eq a
  => GenFunc srid a -> QuadTree srid a -> Quadrant -> a -> QuadTree srid a
grow func tree dir = runIdentity . growM (\q e -> return . func q e) tree dir


generateM
  :: (Monad m, Eq a)
  => GenFuncM m srid a
  -> Extent V2 srid -> a -> m (QuadTree srid a)
generateM func initialExt
  = liftM (flip QuadTree initialExt) . genNodeM func initialExt

genNodeM
  :: forall m srid a. (Eq a, Monad m)
  => GenFuncM m srid a -> Extent V2 srid -> a -> m (Node srid a)
genNodeM func = go
  where
    go :: Extent V2 srid -> a -> m (Node srid a)
    go ext a = liftM (fromMaybe Nil) $ runMaybeT $ do
          let nwext = innerExtent NorthWest ext
              neext = innerExtent NorthEast ext
              swext = innerExtent SouthWest ext
              seext = innerExtent SouthEast ext
          nw <- MaybeT $ func NorthWest nwext a
          ne <- MaybeT $ func NorthEast neext a
          sw <- MaybeT $ func SouthWest swext a
          se <- MaybeT $ func SouthEast seext a
          if nw==ne && ne==sw && sw==se
            then return (Leaf nw)
            else lift $ liftM4 Node
                  (go nwext nw) (go neext ne) (go swext sw) (go seext se)

growM
  :: (Eq a, Monad m)
  => GenFuncM m srid a -> QuadTree srid a -> Quadrant -> a
  -> m (QuadTree srid a)
growM func (QuadTree oldRoot ext) dir val = liftM (flip QuadTree newExt) newRoot
  where
    oldRoot' = return oldRoot
    newRoot
      = case dir of
          NorthWest ->
            liftM4 Node oldRoot' (gen NorthEast) (gen SouthWest) (gen SouthEast)
          NorthEast ->
            liftM4 Node (gen NorthWest) oldRoot' (gen SouthWest) (gen SouthEast)
          SouthWest ->
            liftM4 Node (gen NorthWest) (gen NorthEast) oldRoot' (gen SouthEast)
          SouthEast ->
            liftM4 Node (gen NorthWest) (gen NorthEast) (gen SouthWest) oldRoot'
    newExt  = outerExtent dir ext
    gen dir' = genNodeM func (innerExtent dir' newExt) val


innerExtent :: Quadrant -> Extent V2 srid -> Extent V2 srid
innerExtent q (Extent (V2 x0 y0) (V2 x1 y1))
  = case q of
      NorthWest -> Extent (V2 x0     (y0+h)) (V2 (x0+w) y1    )
      NorthEast -> Extent (V2 (x0+w) (y0+h)) (V2 x1     y1    )
      SouthWest -> Extent (V2 x0      y0   ) (V2 (x0+w) (y0+h))
      SouthEast -> Extent (V2 (x0+w)  y0   ) (V2 x1     (y0+h))
  where w = (x1-x0)/2; h = (y1-y0)/2

outerExtent :: Quadrant -> Extent V2 srid -> Extent V2 srid
outerExtent q (Extent (V2 x0 y0) (V2 x1 y1))
  = case q of
      NorthWest -> Extent (V2 x0     (y0-h)) (V2 (x1+w) y1    )
      NorthEast -> Extent (V2 (x0-w) (y0-h)) (V2 x1     y1    )
      SouthWest -> Extent (V2 x0      y0   ) (V2 (x1+w) (y1+h))
      SouthEast -> Extent (V2 (x0-w)  y0   ) (V2 x1     (y1+h))
  where w = (x1-x0); h = (y1-y0)
