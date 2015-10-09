{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Data.Foldable
import Data.Functor.Identity (runIdentity)

import Data.Primitive.Array
import Control.Monad.ST

import Language.Haskell.TH.Syntax

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree.Internal.TH (machineEpsilonAndLevel)

data QuadTree (v :: * -> *) (srid :: Nat) a
  = QuadTree {
      qtRoot   :: QNode v srid a
    , qtExtent :: {-# UNPACK #-} !(Extent v srid)
    , qtLevel  :: {-# UNPACK #-} !Level
  }

instance (VectorSpace v, Eq a) => Eq (QuadTree v srid a) where
  (==) a b = qtExtent a    == qtExtent b
          && qtLevel  a    == qtLevel b
          && go (qtRoot a) (qtRoot b)
    where
      go (QLeaf _ a') (QLeaf _ b') = a' == b'
      go (QNode _ a') (QNode _ b') = loop 0
        where loop !i
                | i < numChildren (Proxy :: Proxy v)
                , indexArray a' i `go` indexArray b' i = loop (i+1)
                | otherwise                            = True
      go _            _            = False

instance VectorSpace v => Functor (QuadTree v srid) where
  fmap f qt@QuadTree{qtRoot=root} = qt {qtRoot=go rootParent root}
    where
      go p QLeaf{qData=a} = QLeaf {qData=f a, qParent=p}
      go p QNode{qChildren=children}
        = let n = QNode { qChildren = runIdentity (generateChildren genChild)
                        , qParent   = p}
              genChild = return . go n . indexArray children
          in n

generateChildren
  :: forall v srid m a. (VectorSpace v, Monad m)
  => (Int -> m (QNode v srid a)) -> m (Array (QNode v srid a))
generateChildren f = do
  elems <- mapM f [0..(numChildren (Proxy :: Proxy v))-1]
  return $! runST $ do
    cs <- newArray (numChildren (Proxy :: Proxy v)) undefined
    let loop !_ []     = return ()
        loop !i (x:xs) = writeArray cs i x >> loop (i+1) xs
    loop 0 elems
    unsafeFreezeArray cs
{-# INLINE generateChildren #-}

numChildren :: VectorSpace v => Proxy v -> Int
numChildren p = 2 ^ dim p
{-# INLINE numChildren #-}

getChild
  :: VectorSpace v => Array (QNode v srid a) -> Quadrant v -> QNode v srid a
getChild c = runIdentity . indexArrayM c . fromEnum
{-# INLINE getChild #-}


instance VectorSpace v => Foldable (QuadTree v srid) where
  foldMap f qt = foldMap f (qtRoot qt)
  {-# INLINE foldMap #-}


rootParent :: QNode v srid a
rootParent = error "QuadTree: should not happen, tried to get root's parent"


instance VectorSpace v => Show (QuadTree v srid a) where
  show QuadTree{..} = concat ([
     "QuadTree { qtExtent = ", show qtExtent, ","
    ,          " qtLevel = ", show qtLevel, " }"] :: [String])

type Box v = Vertex v

data QNode (v :: * -> *) (srid :: Nat) a
  = QLeaf { qParent   :: QNode v srid a   -- undefined if root
          , qData     :: a
          }
  | QNode { qParent   :: QNode v srid a   -- undefined if root
          , qChildren :: {-# UNPACK #-} !(Array (QNode v srid a))
          }

instance (Show a, VectorSpace v) => Show (QNode v srid a) where
  show QLeaf{..} = concat (["QLeaf {qData = ", show qData, "}"] :: [String])
  show n@QNode{} = concat ([ "QNode {qChildren = "
                           , show (toList n), " }"] :: [String])

instance VectorSpace v => Foldable (QNode v srid) where
  foldMap f QLeaf{qData=a}            = f a
  foldMap f QNode{qChildren=children} = loop 0 mempty
    where loop !i acc
            | i < numChildren (Proxy :: Proxy v)
            = loop (i+1) (acc `mappend` foldMap f (indexArray children i))
            | otherwise = acc
  {-# INLINE foldMap #-}


data Node m v (srid::Nat) a
  = Leaf a
  | Node (Extent v srid -> m (a, Node m v srid a))

data Half = First | Second
  deriving (Show, Eq, Enum, Bounded)

newtype Quadrant v = Quadrant {unQuadrant :: v Half}

instance VectorSpace v => Eq (Quadrant v) where
  Quadrant a == Quadrant b = all id (liftA2 (==) a b)

deriving instance Show (v Half) => Show (Quadrant v)



instance VectorSpace v => Enum (Quadrant v) where
  fromEnum = snd . foldl' go (0::Int,0) . unQuadrant
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

data Neighbor v = Ng { ngPosition :: !(NeighborPosition v)
                     , ngPlanes   :: [HyperPlaneDirections v]
                     }

deriving instance (Show (HyperPlaneDirections v), Show (NeighborPosition v))
  => Show (Neighbor v)


type Neighbors v = [Neighbor v]
type NeighborPosition v = v NeighborDir

oppositePosition :: VectorSpace v => NeighborPosition v -> NeighborPosition v
oppositePosition = fmap (\p -> case p of {Up->Down; Down->Up; _->p})
{-# INLINE oppositePosition #-}


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
  maxBound = Level ((finiteBitSize (undefined::Word) `unsafeShiftR` 1) - 1)
  minBound = Level 0
  {-# INLINE maxBound #-}
  {-# INLINE minBound #-}

qtEpsilon  :: Double
qtEpsLevel :: Int
(qtEpsLevel, qtEpsilon) = $$(machineEpsilonAndLevel 1) :: (Int, Double)
{-# INLINE qtEpsilon #-}
{-# INLINE qtEpsLevel #-}


newtype LocCode v = LocCode {unLocCode :: v Word}

deriving instance Num (v Word) => Num (LocCode v)
deriving instance Eq (v Word) => Eq (LocCode v)
deriving instance Show (v Word) => Show (LocCode v)

newtype QtVertex v = QtVertex {unQtVertex :: Vertex v}
deriving instance VectorSpace v => Num (QtVertex v)
deriving instance VectorSpace v => Show (QtVertex v)
deriving instance VectorSpace v => Eq (QtVertex v)

isVertexNeighbor :: VectorSpace v => Neighbor v -> Bool
isVertexNeighbor = not . any (==Same) . ngPosition
{-# INLINE isVertexNeighbor #-}

neighborsDefault
  :: forall v. HasHyperplanes v => Neighbors v
neighborsDefault = sortBy vertexNeighborsFirst $ do
  n <- replicateM (dim (Proxy :: Proxy v)) [minBound..maxBound]
  guard (not (all (==Same) n))
  let dir = unsafeFromCoords n
  return $! Ng dir (mkDirections dir)
  where
    vertexNeighborsFirst a b
      | isVertexNeighbor a
      , not (isVertexNeighbor b) = LT
      | isVertexNeighbor b
      , not (isVertexNeighbor a) = GT
      | otherwise                = EQ

    mkDirections :: NeighborPosition v -> [HyperPlaneDirections v]
    mkDirections pos = map (unsafeFromCoords . (must++)) combPerhaps
       where
        combPerhaps                 = combinations (numDirs-length must) perhaps
        (_, must, perhaps)          = foldl' makeDirection (0, [], []) pos
        makeDirection (!i,m,p) Same = (i+1, unit i:m, p       )
        makeDirection (!i,m,p) _    = (i+1, m       , unit i:p)
        numDirs                     = dim (Proxy :: Proxy v) - 1
        unit                        = (!!) (coords (identity :: SqMatrix v))


mkNeighbors
  :: forall v. HasHyperplanes v => Q (TExp (Neighbors v))
mkNeighbors = let ns = mapM (fmap unType . liftNeighbor) (neighborsDefault :: Neighbors v)
              in [|| $$(liftM (TExp . ListE) ns) ||]

liftNeighbor
  :: forall v. HasHyperplanes v => Neighbor v -> Q (TExp (Neighbor v))
liftNeighbor (Ng v ns)
  = [|| Ng $$(liftTExp v) (map unsafeFromCoords $$(planes)) ||]
  where
    planes = liftM (TExp . ListE) (mapM mkPlane ns)
    mkPlane :: HyperPlaneDirections v -> Q Exp
    mkPlane p = liftM ListE (mapM (fmap unType . liftTExp) (coords p))
