{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
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
  , Level

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
  , LocCode (..)
  , Level(..)
  , calculateExtent
  , traverseFromLevel
  , locCode
) where


import Control.Monad (liftM)
import Data.Proxy (Proxy(..))
import Data.Bits
import Linear.V (V(V, toVector))
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms (contains)

data QuadTree (srid :: Nat) a
  = QuadTree {
      qtRoot   :: QNode srid a
    , qtExtent :: {-# UNPACK #-} !(Extent V2 srid)
    , qtLevel  :: {-# UNPACK #-} !Level
  } deriving (Show, Functor)


data QNode (srid :: Nat) a
  = QLeaf a
  | QNode (V 4 (QNode srid a))
  deriving (Show, Functor)

data Node m (srid::Nat) a
  = Leaf a
  | Node (Extent V2 srid -> m (a, Node m srid a))


data Halve = First | Second
  deriving (Show, Eq, Enum, Bounded)

newtype Quadrant v = Q {unQ :: v Halve}

deriving instance Eq (v Halve) => Eq (Quadrant v)
deriving instance Show (v Halve) => Show (Quadrant v)

instance VectorSpace v => Enum (Quadrant v) where
  fromEnum
    = V.sum . V.imap (\i qv -> (fromEnum qv `shiftL` i))
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

newtype Level = Level {unLevel :: Int}
  deriving (Eq, Ord, Show, Num)

newtype LocCode v = LocCode {unLocCode :: v Word}

deriving instance Eq (v Word) => Eq (LocCode v)
deriving instance Show (v Word) => Show (LocCode v)


instance Bounded Level where
  maxBound = Level (truncate (logBase 2 maxWord) - 1)
    where maxWord = fromIntegral (maxBound :: Word) :: Double
  minBound = Level 0

maxValue :: Level -> Word
maxValue = (2^) . unLevel

qtMinBox :: QuadTree srid a -> Vertex V2
qtMinBox QuadTree{qtLevel=l, qtExtent=e}
  = fmap (/ (fromIntegral (maxValue l))) (eSize e)

locCode
  :: VectorSpace v
  => Extent v srid -> Level -> Point v srid -> Maybe (LocCode v)
locCode extent level (Point p)
  | Extent (pure 0) (pure 1) `contains` Point p' = Just (LocCode ret)
  | otherwise                                    = Nothing
  where p'  = (p - eMin extent) / eSize extent
        m   = fromIntegral (maxValue level)
        ret = fmap (truncate . (*m)) p'
{-# INLINE locCode #-}

traverseFromLevel
  :: QNode srid a -> Level -> LocCode V2 -> (Level, QNode srid a)
traverseFromLevel node level code = go node level
  where
    go n@(QLeaf _) l   = (l,n)
    go n l | l<1       = (0,n)
    go (QNode (V v)) l = go (v `V.unsafeIndex` ixFromLocCode (l-1) code) (l-1)
{-# INLINE traverseFromLevel #-}

ixFromLocCode
  :: VectorSpace v
  => Level -> LocCode v -> Int
ixFromLocCode (Level l)
  = V.sum . V.imap (\i v -> fromEnum (v `testBit` l) `shiftL` i)
  . toVector . toVectorN . unLocCode
{-# INLINE ixFromLocCode #-}

generate
  :: Monad m
  => Node m srid a -> Extent V2 srid -> Level -> m (QuadTree srid a)
generate build ext level
  | level > maxBound || level < minBound
  = fail "QuadTree.generate: invalid level"
  | otherwise
  = QuadTree <$> genNode ext level build
             <*> pure ext
             <*> pure (max level 0)
generate2
  :: Monad m
  => Node m srid a -> Extent V2 srid -> Vertex V2 -> m (QuadTree srid a)
generate2 build ext minBox = generate build effectiveExt level
  where effectiveExt = Extent (eMin ext) (eMin ext + delta)
        delta  = fmap (* maxVal) minBox 
        maxVal = fromIntegral (maxValue level)
        level  = Level (ceiling (logBase 2 nCells))
        nCells = F.maximum (eSize ext / minBox)


genNode
  :: Monad m
  => Extent V2 srid -> Level -> Node m srid a -> m (QNode srid a)
genNode _   _ (Leaf v) = return (QLeaf v)
genNode ext level (Node f)
  | level > 0 = genNodeQ $ \q -> do
                  next <- liftM snd (f (innerExtent q ext))
                  genNode (innerExtent q ext) (level-1) next
  | otherwise = liftM (QLeaf . fst) (f ext)

genNodeQ :: Monad m => (Quadrant V2 -> m (QNode srid a)) -> m (QNode srid a)
genNodeQ f = liftM (QNode . V) (V.generateM 4 (f . toEnum))

grow
  :: Monad m
  => Node m srid a -> Quadrant V2 -> QuadTree srid a -> m (QuadTree srid a)
grow build dir (QuadTree oldRoot ext oldLevel)
  | oldLevel + 1 > maxBound = fail "QuadTree.grow: cannot grow"
  | otherwise
  = QuadTree <$> newRoot <*> pure newExt <*> pure (oldLevel + 1)
  where
    newRoot = genNodeQ $ \q ->
      if q==dir
        then (return oldRoot)
        else genNode (innerExtent q newExt) oldLevel build
    newExt = outerExtent dir ext

lookupByPoint :: QuadTree srid a -> Point V2 srid -> Maybe (a, Extent V2 srid)
lookupByPoint QuadTree{..} p
  = case locCode qtExtent qtLevel p of
      Just code ->
        let (level, node) = traverseFromLevel qtRoot qtLevel code
            leafCode      = maskCode level code
        in case node of
          QLeaf v -> Just (v, calculateExtent qtExtent qtLevel level leafCode)
          QNode _ -> error "QuadTree.lookupByPoint: traverse gave me a QNode!"
      Nothing -> Nothing
{-# INLINE lookupByPoint #-}

maskCode :: VectorSpace v => Level -> LocCode v -> LocCode v
maskCode (Level l) (LocCode code) = LocCode (fmap (.&. mask) code)
  where mask = complement (U.sum (U.generate l (\i -> (1::Word) `shiftL` i)))

calculateExtent
  :: VectorSpace v
  => Extent v srid -> Level -> Level -> LocCode v -> Extent v srid
calculateExtent maxExtent maxLevel (Level level) (LocCode code)
  = Extent (divIt code) (divIt ((+) <$> code <*> pure (1 `shiftL` level)))
  where
    divIt  = (+ eMin') . (* eSize') . fmap ((/ maxVal) . fromIntegral)
    maxVal = fromIntegral (maxValue maxLevel)
    eMin'  = eMin maxExtent
    eSize' = eSize maxExtent
{-# INLINE calculateExtent #-}

traceRay :: QuadTree srid a -> Point V2 srid -> Point V2 srid -> [a]
traceRay qt from@(Point (V2 x0 y0)) to@(Point (V2 x1 y1))
  = reverse (go from [])
  where
    go a ps
      = case lookupByPoint qt a of
          Nothing                           -> ps
          Just (v, ext) | ext `contains` to -> v:ps
          Just (v, ext@(Extent (V2 _ miny) (V2 _ maxy))) ->
            let (xx, xy) = xIntersect a ext
                (yx, yy) = yIntersect a ext
                next
                  | miny <= yy && yy < maxy = Point (V2 yx yy)
                  | otherwise               = Point (V2 xx xy)
            in go next (v:ps)
    slope = (y1-y0) / (x1-x0)
    epsilon = 1e-6
    xIntersect (Point (V2 x y)) (Extent (V2 _ miny) (V2 _ maxy))
      | abs(x1-x0) < epsilon = (x, y')
      | otherwise            = (x + ((y' - y) / slope), y')
      where y' = if y1>y0 then maxy else miny-epsilon
    yIntersect (Point (V2 x y)) (Extent (V2 minx _) (V2 maxx _))
      | abs(y1-y0) < epsilon = (x', y)
      | otherwise            = (x', y + ((x' - x) * slope))
      where x' = if x1>x0 then maxx else minx-epsilon
{-# INLINE traceRay #-}


innerExtent :: Quadrant V2 -> Extent V2 srid -> Extent V2 srid
innerExtent q (Extent (V2 x0 y0) (V2 x1 y1))
  = case q of
      (Q (V2 First First))   -> Extent (V2 x0      y0   ) (V2 (x0+w) (y0+h))
      (Q (V2 First Second))  -> Extent (V2 x0     (y0+h)) (V2 (x0+w) y1    )
      (Q (V2 Second First))  -> Extent (V2 (x0+w)  y0   ) (V2 x1     (y0+h))
      (Q (V2 Second Second)) -> Extent (V2 (x0+w) (y0+h)) (V2 x1     y1    )
  where w = (x1-x0)/2; h = (y1-y0)/2
{-# INLINE innerExtent #-}

outerExtent :: Quadrant V2 -> Extent V2 srid -> Extent V2 srid
outerExtent q (Extent (V2 x0 y0) (V2 x1 y1))
  = case q of
      (Q (V2 First First))   -> Extent (V2 x0      y0   ) (V2 (x1+w) (y1+h))
      (Q (V2 First Second))  -> Extent (V2 x0     (y0-h)) (V2 (x1+w) y1    )
      (Q (V2 Second First))  -> Extent (V2 (x0-w)  y0   ) (V2 x1     (y1+h))
      (Q (V2 Second Second)) -> Extent (V2 (x0-w) (y0-h)) (V2 x1     y1    )
  where w = (x1-x0); h = (y1-y0)
{-# INLINE outerExtent #-}


{-
lookupByPoint :: QuadTree srid a -> Point V2 srid -> Maybe (a, Extent V2 srid)
lookupByPoint qt p = go (qtRoot qt) (qtExtent qt)
  where
    getQuadrant q (QNode (V v)) = v `V.unsafeIndex` (fromEnum q)
    getQuadrant _ (QLeaf _)     = error "getQuadrant called on QLeaf"
    go (QLeaf v) ext
      | ext `contains` p = Just (v,ext)
      | otherwise        = Nothing
    go node ext = let es = dropWhile
                           (\(e,_) -> not (e `contains` p))
                           (map (\q -> (innerExtent q ext, q)) [minBound..maxBound])
                  in case es of
                    []        -> Nothing
                    ((e,q):_) -> go (getQuadrant q node) e
{-# INLINE lookupByPoint #-}
-}
