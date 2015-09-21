{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Sigym4.Geometry.QuadTree (
    QuadTree
  , Quadrant (..)
  , TreeBuilder (..)

  , generate
  , grow

  , lookupByPoint
  , traceRay

  , qtExtent

  -- | internal (exposed for testing)
  , innerExtent
  , outerExtent
) where


import Control.Monad (liftM, liftM4)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)

import Sigym4.Geometry (Nat, Extent(..), Point(..), V2(..))
import Sigym4.Geometry.Algorithms (contains)

data QuadTree (srid :: Nat) a
  = QuadTree {
      qtRoot   :: Node srid a
    , qtExtent :: !(Extent V2 srid)
  } deriving (Show, Functor)

data Quadrant = NorthWest | NorthEast | SouthWest | SouthEast
  deriving (Show, Eq, Enum, Bounded)


data Node (srid :: Nat) a
  = Leaf a
  | Node { qNW     :: Node srid a
         , qNE     :: Node srid a
         , qSW     :: Node srid a
         , qSE     :: Node srid a
         }
  deriving (Show, Functor)

data TreeBuilder m (srid::Nat) a
  = CreateLeaf a
  | CreateNode (Extent V2 srid -> m (TreeBuilder m srid a))

generate
  :: Monad m
  => TreeBuilder m srid a -> Extent V2 srid -> m (QuadTree srid a)
generate build ext = liftM (flip QuadTree ext) (genNode ext build)

genNode
  :: Monad m
  => Extent V2 srid -> TreeBuilder m srid a -> m (Node srid a)
genNode _   (CreateLeaf v) = return (Leaf v)
genNode ext (CreateNode f) = do
  let nw = innerExtent NorthWest ext
      ne = innerExtent NorthEast ext
      sw = innerExtent SouthWest ext
      se = innerExtent SouthEast ext
  liftM4 Node (f nw >>= genNode nw)
              (f ne >>= genNode ne)
              (f sw >>= genNode sw)
              (f se >>= genNode se)

grow
  :: Monad m
  => TreeBuilder m srid a -> Quadrant -> QuadTree srid a -> m (QuadTree srid a)
grow build dir (QuadTree oldRoot ext)
  = liftM (flip QuadTree newExt) newRoot
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
    newExt   = outerExtent dir ext
    gen dir' = genNode (innerExtent dir' newExt) build

lookupByPoint :: QuadTree srid a -> Point V2 srid -> Maybe (a, Extent V2 srid)
lookupByPoint qt p = go (qtRoot qt) (qtExtent qt)
  where
    go (Leaf v) ext
      | ext `contains` p = Just (v,ext)
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
{-# INLINE lookupByPoint #-}

traceRay :: QuadTree srid a -> Point V2 srid -> Point V2 srid -> [a]
traceRay qt from@(Point (V2 x0 y0)) to@(Point (V2 x1 y1))
  = reverse (go from [])
  where
    go a ps
      = case lookupByPoint qt a of
          Nothing                           -> ps
          Just (v, ext) | ext `contains` to -> v:ps
          Just (v, ext@(Extent (V2 minx _) (V2 maxx _))) ->
            let (xx, xy) = xIntersect a ext
                (yx, yy) = yIntersect a ext
                next
                  | minx <= xx && xx < maxx = Point (V2 xx xy)
                  | otherwise               = Point (V2 yx yy)
            in go next (v:ps)
    slope = (y1-y0) / (x1-x0)
    epsilon = 1e-6
    delta   = 1e-1
    xIntersect (Point (V2 x y)) (Extent (V2 _ miny) (V2 _ maxy))
      | abs(x1-x0) < epsilon = (x, y')
      | otherwise            = (x + ((y' - y) / slope), y')
      where y' = if y1>y0 then maxy else miny-delta
    yIntersect (Point (V2 x y)) (Extent (V2 minx _) (V2 maxx _))
      | abs(y1-y0) < epsilon = (x', y)
      | otherwise            = (x', y + ((x' - x) * slope))
      where x' = if x1>x0 then maxx else minx-delta
{-# INLINE traceRay #-}


innerExtent :: Quadrant -> Extent V2 srid -> Extent V2 srid
innerExtent q (Extent (V2 x0 y0) (V2 x1 y1))
  = case q of
      NorthWest -> Extent (V2 x0     (y0+h)) (V2 (x0+w) y1    )
      NorthEast -> Extent (V2 (x0+w) (y0+h)) (V2 x1     y1    )
      SouthWest -> Extent (V2 x0      y0   ) (V2 (x0+w) (y0+h))
      SouthEast -> Extent (V2 (x0+w)  y0   ) (V2 x1     (y0+h))
  where w = (x1-x0)/2; h = (y1-y0)/2
{-# INLINE innerExtent #-}

outerExtent :: Quadrant -> Extent V2 srid -> Extent V2 srid
outerExtent q (Extent (V2 x0 y0) (V2 x1 y1))
  = case q of
      NorthWest -> Extent (V2 x0     (y0-h)) (V2 (x1+w) y1    )
      NorthEast -> Extent (V2 (x0-w) (y0-h)) (V2 x1     y1    )
      SouthWest -> Extent (V2 x0      y0   ) (V2 (x1+w) (y1+h))
      SouthEast -> Extent (V2 (x0-w)  y0   ) (V2 x1     (y1+h))
  where w = (x1-x0); h = (y1-y0)
{-# INLINE outerExtent #-}
