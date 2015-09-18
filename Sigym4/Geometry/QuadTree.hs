{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Sigym4.Geometry.QuadTree (
    QuadTree
  , Quadrant (..)

  , generate
  , grow
  , lookupPoint
  , qtExtent

  -- | internal (exposed for testing)
  , innerExtent
  , outerExtent
) where

import Data.Maybe (fromMaybe)

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

generate
  :: Eq a
  => GenFunc srid a
  -> Extent V2 srid -> a -> QuadTree srid a
generate func initialExt = flip QuadTree initialExt . genNode func initialExt

genNode
  :: forall srid a. Eq a
  => GenFunc srid a -> Extent V2 srid -> a -> Node srid a
genNode func = go
  where
    go :: Extent V2 srid -> a -> Node srid a
    go ext a = fromMaybe Nil $ do
          let nwext = innerExtent NorthWest ext
              neext = innerExtent NorthEast ext
              swext = innerExtent SouthWest ext
              seext = innerExtent SouthEast ext
          nw <- func NorthWest nwext a
          ne <- func NorthEast neext a
          sw <- func SouthWest swext a
          se <- func SouthEast seext a
          if nw==ne && ne==sw && sw==se
            then return (Leaf nw)
            else return Node { qNW = go nwext nw
                             , qNE = go neext ne
                             , qSW = go swext sw
                             , qSE = go seext se
                             }

grow
  :: Eq a
  => GenFunc srid a -> QuadTree srid a -> Quadrant -> a -> QuadTree srid a
grow func (QuadTree oldRoot ext) dir val = QuadTree newRoot newExt
  where
    newRoot = case dir of
                NorthWest -> Node { qNW = oldRoot
                                  , qNE = gen NorthEast
                                  , qSE = gen SouthEast
                                  , qSW = gen SouthWest}

                NorthEast -> Node { qNW = gen NorthWest
                                  , qNE = oldRoot
                                  , qSE = gen SouthEast
                                  , qSW = gen SouthWest}

                SouthEast -> Node { qNW = gen NorthWest
                                  , qNE = gen NorthEast
                                  , qSE = oldRoot
                                  , qSW = gen SouthWest}

                SouthWest -> Node { qNW = gen NorthWest
                                  , qNE = gen NorthEast
                                  , qSE = gen SouthEast
                                  , qSW = oldRoot}
    newExt  = outerExtent dir ext
    gen dir' = genNode func (innerExtent dir' newExt) val


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
