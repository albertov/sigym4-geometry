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

module Sigym4.Geometry.QuadTree (
    QuadTree
  , Quadrant (..)
  , Halve (..)
  , Node (..)
  , Level (Level) -- constructor is internal, exposed for tests

  , generate
  , generate2
  , grow

  , lookupByPoint
  , traceRay

  , qtExtent
  , qtLevel
  , qtMinBox

  -- | internal (exposed for testing)
  , innerExtent
  , outerExtent
) where


import Control.Applicative ((<$>), (<*>), pure, liftA2)
import Control.Monad (liftM)
import Data.Proxy (Proxy(..))
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms (contains)

import GHC.TypeLits

data QuadTree v (srid :: Nat) a
  = QuadTree {
      qtRoot   :: QNode v srid a
    , qtExtent :: {-# UNPACK #-} !(Extent v srid)
    , qtLevel  :: {-# UNPACK #-} !Level
  } deriving (Show, Functor)


data QNode v (srid :: Nat) a
  = QLeaf a
  | QNode (V (2 ^ VsDim v) (QNode v srid a))
  deriving (Show, Functor)

data Node m v (srid::Nat) a
  = Leaf a
  | Node (Extent v srid -> m (a, Node m v srid a))


data Halve = First | Second
  deriving (Show, Eq, Enum, Bounded)

newtype Quadrant v = Q {unQ :: v Halve}

instance VectorSpace v => Eq (Quadrant v) where
  Q a == Q b = F.all id (liftA2 (==) a b)

deriving instance Show (v Halve) => Show (Quadrant v)

instance VectorSpace v => Enum (Quadrant v) where
  fromEnum
    = V.sum . V.imap (\i qv -> (fromEnum qv `unsafeShiftL` i))
    . toVector . toVectorN . unQ
  {-# INLINE fromEnum #-}

  toEnum ix
    = Q . fromVectorN . V
    $ V.generate d (\i -> if ix `testBit` i then Second else First)
    where d = dim (Proxy :: Proxy v)
  {-# INLINE toEnum #-}

instance VectorSpace v => Bounded (Quadrant v) where
  minBound = Q (pure First)
  {-# INLINE minBound #-}
  maxBound = Q (pure Second)
  {-# INLINE maxBound #-}

newtype Level = Level Int
  deriving (Eq, Ord, Show, Num)

newtype LocCode v = LocCode {unLocCode :: v Word}

deriving instance Eq (v Word) => Eq (LocCode v)
deriving instance Show (v Word) => Show (LocCode v)


instance Bounded Level where
  maxBound = Level (finiteBitSize (undefined :: Word) - 1)
  minBound = Level 0

maxValue :: Level -> Word
maxValue (Level l) = 2 ^ l

qtMinBox :: VectorSpace v => QuadTree v srid a -> Vertex v
qtMinBox QuadTree{qtLevel=l, qtExtent=e}
  = fmap (/ (fromIntegral (maxValue l))) (eSize e)

generate
  :: (Monad m, VectorSpace v)
  => Node m v srid a -> Extent v srid -> Level -> m (QuadTree v srid a)
generate build ext level
  | level > maxBound || level < minBound
  = fail "QuadTree.generate: invalid level"
  | otherwise
  = QuadTree <$> genNode ext level build
             <*> pure ext
             <*> pure (max level 0)
generate2
  :: (Monad m, VectorSpace v)
  => Node m v srid a -> Extent v srid -> Vertex v -> m (QuadTree v srid a)
generate2 build ext minBox = generate build effectiveExt level
  where effectiveExt = Extent (eMin ext) (eMin ext + delta)
        delta  = fmap (* maxVal) minBox 
        maxVal = fromIntegral (maxValue level)
        level  = Level (ceiling (logBase 2 nCells))
        nCells = F.maximum (eSize ext / minBox)


genNode
  :: (Monad m, VectorSpace v)
  => Extent v srid -> Level -> Node m v srid a -> m (QNode v srid a)
genNode _   _ (Leaf v) = return (QLeaf v)
genNode ext level (Node f)
  | level > 0 = genQNode $ \q -> do
                  next <- liftM snd (f (innerExtent q ext))
                  genNode (innerExtent q ext) (level-1) next
  | otherwise = liftM (QLeaf . fst) (f ext)

genQNode
  :: forall m v srid a. (Monad m, VectorSpace v)
  => (Quadrant v -> m (QNode v srid a)) -> m (QNode v srid a)
genQNode f = liftM (QNode . V) (V.generateM numNodes (f . toEnum))
  where numNodes = 2 ^ dim (Proxy :: Proxy v) 

grow
  :: (Monad m, VectorSpace v)
  => Node m v srid a -> Quadrant v -> QuadTree v srid a -> m (QuadTree v srid a)
grow build dir (QuadTree oldRoot ext oldLevel)
  | oldLevel + 1 > maxBound = fail "QuadTree.grow: cannot grow"
  | otherwise
  = QuadTree <$> newRoot <*> pure newExt <*> pure (oldLevel + 1)
  where
    newRoot = genQNode $ \q ->
      if q == dir
        then (return oldRoot)
        else genNode (innerExtent q newExt) oldLevel build
    newExt = outerExtent dir ext


qtLocCode
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe (LocCode v)
qtLocCode QuadTree{qtExtent=extent, qtLevel=level} (Point p)
  | Extent (pure 0) (pure 1) `contains` Point p' = Just (LocCode ret)
  | otherwise                                    = Nothing
  where p'  = (p - eMin extent) / eSize extent
        m   = fromIntegral (maxValue level)
        ret = fmap (truncate . (*m)) p'
{-# INLINE qtLocCode #-}

traverseFromLevel
  :: VectorSpace v
  => QNode v srid a -> Level -> LocCode v -> (QNode v srid a, Level)
traverseFromLevel node level code = go node level
  where
    go n@(QLeaf _) l   = (n,l)
    go n l | l<1       = (n,0)
    go (QNode (V v)) l = go (v `V.unsafeIndex` ixFromLocCode (l-1) code) (l-1)
{-# INLINE traverseFromLevel #-}

ixFromLocCode
  :: VectorSpace v
  => Level -> LocCode v -> Int
ixFromLocCode (Level l)
  = V.foldl' (.|.) 0
  . V.imap (\i v -> fromEnum (v `testBit` l) `unsafeShiftL` i)
  . toVector . toVectorN . unLocCode
{-# INLINE ixFromLocCode #-}

lookupByPoint
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe (a, Extent v srid)
lookupByPoint qt@QuadTree{..} p
  = case qtLocCode qt p of
      Just code ->
        let (node, level) = traverseFromLevel qtRoot qtLevel code
        in case node of
          QLeaf v -> Just (v, calculateExtent qtExtent qtLevel level code)
          QNode _ -> error "QuadTree.lookupByPoint: traverse gave me a QNode!"
      Nothing -> Nothing
{-# INLINE lookupByPoint #-}

calculateExtent
  :: VectorSpace v
  => Extent v srid -> Level -> Level -> LocCode v -> Extent v srid
calculateExtent maxExtent maxLevel (Level level) (LocCode code)
  = Extent (divIt leafCode) (divIt ((+) <$> leafCode <*> pure (bit level)))
  where
    divIt     = (+ eMin') . (* eSize') . fmap ((/ maxVal) . fromIntegral)
    maxVal    = fromIntegral (maxValue maxLevel)
    eMin'     = eMin maxExtent
    eSize'    = eSize maxExtent
    leafCode
      | level == 0 = code
      | otherwise  = fmap (.&. mask) code
    mask = complement (U.sum (U.generate level bit))
{-# INLINE calculateExtent #-}

traceRay :: QuadTree V2 srid a -> Point V2 srid -> Point V2 srid -> [a]
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
    delta = F.minimum (qtMinBox qt) / 2
    xIntersect (Point (V2 x y)) (Extent (V2 _ miny) (V2 _ maxy))
      | abs(x1-x0) < epsilon = (x, y')
      | otherwise            = (x + ((y' - y) / slope), y')
      where y' = if y1>y0 then maxy else miny-delta
    yIntersect (Point (V2 x y)) (Extent (V2 minx _) (V2 maxx _))
      | abs(y1-y0) < epsilon = (x', y)
      | otherwise            = (x', y + ((x' - x) * slope))
      where x' = if x1>x0 then maxx else minx-delta
{-# INLINE traceRay #-}


innerExtent :: VectorSpace v => Quadrant v -> Extent v srid -> Extent v srid
innerExtent (Q qv) e = Extent eMin' eMax'
  where
    eMin' = fromVectorN $ V $ V.zipWith3 buildMin vh lo hi 
    eMax' = fromVectorN $ V $ V.zipWith3 buildMax vh lo hi 
    buildMin First  l _ = l
    buildMin Second l h = l + ((h-l)/2)
    buildMax First  l h = l + ((h-l)/2)
    buildMax Second _ h = h
    V lo = toVectorN (eMin e)
    V hi = toVectorN (eMax e)
    V vh = toVectorN qv
{-# INLINE innerExtent #-}


outerExtent :: VectorSpace v => Quadrant v -> Extent v srid -> Extent v srid
outerExtent (Q qv) e = Extent eMin' eMax'
  where
    eMin' = fromVectorN $ V $ V.zipWith3 buildMin vh lo hi 
    eMax' = fromVectorN $ V $ V.zipWith3 buildMax vh lo hi 
    buildMin First  l _ = l
    buildMin Second l h = 2*l - h
    buildMax First  l h = 2*h - l
    buildMax Second _ h = h
    V lo = toVectorN (eMin e)
    V hi = toVectorN (eMax e)
    V vh = toVectorN qv
{-# INLINE outerExtent #-}
