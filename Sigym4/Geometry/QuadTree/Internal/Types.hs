{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sigym4.Geometry.QuadTree.Internal.Types where


import Control.Applicative ((<$>), (<*>), pure, liftA2)
import Control.Monad (guard)
import Data.Proxy (Proxy(..))
import Data.Bits
import Data.List (sortBy)
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Language.Haskell.TH.Syntax

import Sigym4.Geometry

import GHC.TypeLits


data QuadTree v (srid :: Nat) a
  = QuadTree {
      qtRoot   :: QNode v srid a
    , qtExtent :: {-# UNPACK #-} !(Extent v srid)
    , qtLevel  :: {-# UNPACK #-} !Level
  } deriving (Show)


data QNode v (srid :: Nat) a
  = QLeaf { qParent   :: QNode v srid a   -- undefined if root
          , qData     :: a
          }
  | QNode { qParent   :: QNode v srid a   -- undefined if root
          , qChildren :: V (2 ^ VsDim v) (QNode v srid a)
          }

instance Show a => Show (QNode v srid a) where
  show QLeaf{..} = concat (["QLeaf {qData = ", show qData, "}"] :: [String])
  show QNode{..} = concat (["QNode {qChildren = ", show qChildren, "}"] :: [String])

data Node m v (srid::Nat) a
  = Leaf a
  | Node (Extent v srid -> m (a, Node m v srid a))

data Half = First | Second
  deriving (Show, Eq, Enum, Bounded)

newtype Quadrant v = Quadrant {unQuadrant :: v Half}

instance VectorSpace v => Eq (Quadrant v) where
  Quadrant a == Quadrant b = F.all id (liftA2 (==) a b)

deriving instance Show (v Half) => Show (Quadrant v)



instance VectorSpace v => Enum (Quadrant v) where
  fromEnum
    = V.sum . V.imap (\i qv -> (fromEnum qv `unsafeShiftL` i))
    . toVector . toVectorN . unQuadrant
  {-# INLINE fromEnum #-}

  toEnum ix
    = Quadrant . fromVectorN . V
    $ V.generate d (\i -> if ix `testBit` i then Second else First)
    where d = dim (Proxy :: Proxy v)
  {-# INLINE toEnum #-}

instance VectorSpace v => Bounded (Quadrant v) where
  minBound = Quadrant (pure First)
  {-# INLINE minBound #-}
  maxBound = Quadrant (pure Second)
  {-# INLINE maxBound #-}

data NeighborDir = Down | Same | Up
  deriving (Show, Eq, Enum, Bounded)

newtype Neighbor v = Ng {unNg :: v NeighborDir}

type Neighbors v = [Neighbor v]

deriving instance Show (v NeighborDir) => Show (Neighbor v)

instance VectorSpace v => Bounded (Neighbor v) where
  minBound = Ng (pure Down)
  {-# INLINE minBound #-}
  maxBound = Ng (pure Up)
  {-# INLINE maxBound #-}


neighbors :: forall v. VectorSpace v => Neighbors v
neighbors = sortBy vertexNeighborsFirst $ do
  n <- V.replicateM (dim (Proxy :: Proxy v)) [minBound..maxBound]
  guard (not (V.all (==Same) n))
  return $! Ng (fromVectorN (V n))
  where
    vertexNeighborsFirst a b
      | not (hasSame a), hasSame b = LT
      | hasSame a, not (hasSame b) = GT
      | otherwise                  = EQ
    hasSame = F.any (==Same) . unNg
{-# NOINLINE neighbors #-}


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



newtype Level = Level {unLevel :: Int}
  deriving (Eq, Ord, Show)


instance Num Level where
  Level a + Level b
    | Level (a+b) <= maxBound = Level (a+b)
    | otherwise               = error ("Level " ++ show (a+b) ++ " too large")
  Level a - Level b
    | Level (a-b) >= minBound = Level (a-b)
    | otherwise               = error ("Level " ++ show (a-b) ++ " too small")
  Level a * Level b
    | Level (a*b) <= maxBound = Level (a*b)
    | otherwise               = error ("Level " ++ show (a*b) ++ " too large")
  negate                      = error "Level cannot be negative"
  abs                         = id
  signum  (Level 0)           = 0
  signum  _                   = 1
  fromInteger i
    | minBound<=l,l<=maxBound = l
    | otherwise               = error ("Invalid Level " ++ show i)
    where l = Level (fromInteger i)

instance Bounded Level where
  maxBound = Level (finiteBitSize (undefined :: Word) - 1)
  minBound = Level 0


newtype LocCode v = LocCode {unLocCode :: v Word}

deriving instance Eq (v Word) => Eq (LocCode v)
deriving instance Show (v Word) => Show (LocCode v)

newtype QtVertex v = QtVertex {unQtVertex :: Vertex v}
deriving instance VectorSpace v => Show (QtVertex v)
deriving instance VectorSpace v => Eq (QtVertex v)
