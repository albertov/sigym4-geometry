{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
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

import Control.Monad (liftM, replicateM, guard)
import Control.Applicative ((<$>), (<*>), pure, liftA2)
import Linear.Matrix (identity)
import Data.Bits
import Data.List (sortBy)
import Data.Proxy (Proxy(..))
import qualified Data.Foldable as F

import Language.Haskell.TH.Syntax

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms

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
  fromEnum = snd . F.foldl' go (0::Int,0) . unQuadrant
    where go (!i,!s) v = (i+1, s + (fromEnum v `unsafeShiftL` i))
  {-# INLINE fromEnum #-}

  toEnum ix
    = Quadrant . unsafeFromCoords
    $ map (\i -> if ix `testBit` i then Second else First) [0..d-1]
    where d = dim (Proxy :: Proxy v)
  {-# INLINE toEnum #-}

instance VectorSpace v => Bounded (Quadrant v) where
  minBound = Quadrant (pure First)
  {-# INLINE minBound #-}
  maxBound = Quadrant (pure Second)
  {-# INLINE maxBound #-}

data NeighborDir = Down | Same | Up
  deriving (Show, Eq, Enum, Bounded)

instance Lift (NeighborDir) where
    lift Up   = [| Up |]
    lift Down = [| Down |]
    lift Same = [| Same |]

data Neighbor v = Ng { ngDirection :: !(v NeighborDir)
                     , ngNormals   :: !(V (VsDim v - 1) (Direction v))
                     }

type Neighbors v = [Neighbor v]

deriving instance (Show (v NeighborDir), Show (v Double)) => Show (Neighbor v)



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


neighborsDefault
  :: forall v. (VectorSpace v, KnownNat (VsDim v -1))
  => Neighbors v
neighborsDefault = sortBy vertexNeighborsFirst $ do
  n <- replicateM (dim (Proxy :: Proxy v)) [minBound..maxBound]
  guard (not (all (==Same) n))
  let dir = unsafeFromCoords n
  return $! Ng dir (mkNormals dir)
  where
    vertexNeighborsFirst a b
      | not (hasSame a), hasSame b = LT
      | hasSame a, not (hasSame b) = GT
      | otherwise                  = EQ
    hasSame = F.any (==Same) . ngDirection

    mkNormals :: v NeighborDir -> V (VsDim v - 1) (Direction v) 
    mkNormals dir = unsafeFromCoords (take numPlanes (must++perhaps))
       where
        (_, must, perhaps)       = F.foldl' makeNormal (0, [], []) dir
        makeNormal (!i,m,p) Same = (i+1, unit i:m, p       )
        makeNormal (!i,m,p) _    = (i+1, m       , unit i:p)
        numPlanes                = dim (Proxy :: Proxy v) - 1
        unit                     = (!!) (coords (identity :: SqMatrix v))


mkNeighbors
  :: forall v. (VectorSpace v, KnownNat (VsDim v -1)) => Q (TExp (Neighbors v))
mkNeighbors = unsafeTExpCoerce $ liftM ListE $
  mapM liftNeighbor (neighborsDefault :: Neighbors v)


liftNeighbor
  :: forall v. (VectorSpace v, KnownNat (VsDim v - 1)) => Neighbor v -> Q Exp
liftNeighbor (Ng v ns) =
  [| Ng (unsafeFromCoords dirs) (unsafeFromCoords $planes) |]
  where
    planes = liftM ListE (mapM (\n -> [|unsafeFromCoords n|]) ls)
    dirs   = coords v
    ls     = map coords (coords ns)
