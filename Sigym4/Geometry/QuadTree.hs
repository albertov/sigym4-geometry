{-# LANGUAGE CPP #-}
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

module Sigym4.Geometry.QuadTree (
    QuadTree
  , Quadrant (..)
  , Halve (..)
  , Node (..)
  , Level (unLevel)
  , Neighbor
  , NeighborDir (..)

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
  , unsafeQtLocCode
  , qtLocCode
  , outerExtent
  , neighbors
  , neighborsToCheck
  , mkNeighborChecker
) where


import Control.Applicative ((<$>), (<*>), pure, liftA2, liftA3)
import Control.Exception (assert)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Monad (liftM, guard)
import Data.Proxy (Proxy(..))
import Data.Bits
import Data.List (sortBy)
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms

import GHC.TypeLits

#if DEBUG
import Debug.Trace
#endif

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

data NeighborDir = Down | Same | Up
  deriving (Show, Eq, Enum, Bounded)

newtype Neighbor v = Ng {unNg :: v NeighborDir}

deriving instance Show (v NeighborDir) => Show (Neighbor v)

instance VectorSpace v => Bounded (Neighbor v) where
  minBound = Ng (pure Down)
  {-# INLINE minBound #-}
  maxBound = Ng (pure Up)
  {-# INLINE maxBound #-}

neighbors :: forall v. VectorSpace v => [Neighbor v]
neighbors = do
  n <- V.replicateM (dim (Proxy :: Proxy v)) [minBound..maxBound]
  guard (not (V.all (==Same) n))
  return (Ng (fromVectorN (V n)))

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
             <*> pure level
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
qtLocCode QuadTree{qtExtent=ext, qtLevel=level} (Point p)
  | Extent (pure 0) (pure 1) `contains` Point p' = Just (LocCode ret)
  | otherwise                                    = Nothing
  where p'  = (p - eMin ext) / eSize ext
        m   = fromIntegral (maxValue level)
        ret = fmap (truncate . (*m)) p'
{-# INLINE qtLocCode #-}

unsafeQtLocCode
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> LocCode v
unsafeQtLocCode QuadTree{qtExtent=ext, qtLevel=level} (Point p)
  = LocCode (fmap (truncate . (*m)) p')
  where p'  = (p - eMin ext) / eSize ext
        m   = fromIntegral (maxValue level)
{-# INLINE unsafeQtLocCode #-}

traverseToLevel
  :: VectorSpace v
  => QNode v srid a -> Level -> Level -> LocCode v
  -> (QNode v srid a, Level, LocCode v)
traverseToLevel node start end code = go (node, start, cellCode start)
  where
    go nl@((QLeaf _),_,_)   = nl
    go nl@(_,l,_) | l<=end  = nl
    go nl@(QNode (V v),l,_) = let n' = v `V.unsafeIndex` ix
                                  ix = ixFromLocCode l' code
                                  l' = l - 1
                              in go (n',l',cellCode l')
    cellCode (Level l)
      | l == 0     = code
      | otherwise  = LocCode (fmap (.&. mask) (unLocCode code))
      where mask = complement (U.sum (U.generate l bit))
{-# INLINE traverseToLevel #-}

{-
traverseToLevel
  :: VectorSpace v
  => QNode v srid a -> Level -> Level -> LocCode v
  -> [(QNode v srid a, Level, LocCode v)]
traverseToLevel node start end code = go (node, start, cellCode start) []
  where
    go nl@((QLeaf _),_,_)   acc           = nl:acc
    go nl@(_,l,_)           acc | l<=end  = nl:acc
    go nl@(QNode (V v),l,_) acc           = let n' = v `V.unsafeIndex` ix
                                                ix = ixFromLocCode l' code
                                                l' = l - 1
                                            in go (n',l',cellCode l') (nl:acc)
    cellCode (Level l)
      | l == 0     = code
      | otherwise  = LocCode (fmap (.&. mask) (unLocCode code))
      where mask = complement (U.sum (U.generate l bit))
{-# INLINE traverseToLevel #-}
-}

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
      Just c ->
        let (node,level,cellCode) = traverseToLevel qtRoot qtLevel 0 c
            ext           = calculateExtent qtExtent qtLevel level cellCode
        in Just (leafValue node, ext)
      Nothing -> Nothing
{-# INLINE lookupByPoint #-}

leafValue :: QNode v srid a -> a
leafValue (QLeaf v) = v
leafValue _         = error "expected a leaf"
{-# INLINE leafValue #-}

calculateExtent
  :: VectorSpace v
  => Extent v srid -> Level -> Level -> LocCode v -> Extent v srid
calculateExtent maxExtent maxLevel (Level level) (LocCode code)
  = Extent (divIt code) (divIt ((+) <$> code <*> pure (bit level)))
  where
    divIt     = (+ eMin') . (* eSize') . fmap ((/ maxVal) . fromIntegral)
    maxVal    = fromIntegral (maxValue maxLevel)
    eMin'     = eMin maxExtent
    eSize'    = eSize maxExtent
{-# INLINE calculateExtent #-}

traceRay :: forall v srid a. (VectorSpace v, KnownNat (VsDim v - 1), Show a, Show (v Word), Eq (v Word), Show (v NeighborDir))
         => QuadTree v srid a -> Point v srid -> Point v srid -> [a]
traceRay qt@QuadTree{..} from@(Point fromV) to@(Point toV)
  | from == to = []
  | otherwise  = case qtLocCode qt from of
#if DEBUG
                   _ | traceShow ("trace from:",qtExtent,qtLevel,from,to) False -> undefined
#endif
                   Just code -> go code
                   Nothing   -> []
  where
    go code
      | ext `contains` to  = [val]
      | isNothing mNext    = [val]
      | otherwise          = val:(go (fromJust mNext))
      where
        (node, level, cellCode) = traverseToLevel qtRoot qtLevel 0 code
        ext           = calculateExtent qtExtent qtLevel level cellCode
        val           = leafValue node
        (neigh, isec) = getIntersection ext
        --LocCode iseccode = cellLocCode (unsafeQtLocCode qt intersection) level
        LocCode iseccode = unsafeQtLocCode qt isec
        mNext = fmap LocCode (sequence (liftA3 mkNext (unNg neigh) iseccode (unLocCode cellCode)))
        mkNext Down _ c
          | c==0      = Nothing
          | otherwise = Just (c-1)
        mkNext Up   _ c
          | (c+cellSize) < maxCode = Just (c+cellSize)
          | otherwise              = Nothing
        mkNext Same i _ = Just i
        cellSize        = bit (unLevel level)
        maxCode         = bit (unLevel qtLevel)

    getIntersection :: Extent v srid -> (Neighbor v, Point v srid)
    getIntersection ext = case catMaybes matches of
                           [] -> error "no matches"
                           m  -> head m
      where matches = map ($ ext) checkers

    toCheck = neighborsToCheck fromV toV
    checkers = map (mkNeighborChecker from to) toCheck

{-
traceRay :: forall v srid a. (VectorSpace v, KnownNat (VsDim v - 1), Show a, Show (v Word), Eq (v Word), Show (v NeighborDir))
         => QuadTree v srid a -> Point v srid -> Point v srid -> [a]
traceRay qt@QuadTree{..} from@(Point fromV) to@(Point toV)
  | from == to = []
  | otherwise  = case qtLocCode qt from of
#if DEBUG
                   _ | traceShow ("trace from:",qtExtent,qtLevel,from,to) False -> undefined
#endif
                   Just code -> go (traverseToLevel qtRoot qtLevel 0 code)
                   Nothing   -> []
  where
    go path
      | null path          = []
#if DEBUG
      | traceShow ("go", head path, next, length ancestorPath) False = undefined
#endif
      | null ancestorPath  = [val]
      | ext `contains` to  = [val]
      | otherwise          = val:(go nextPath)
      where
        (node,level,cellCode) = head path
        ext           = calculateExtent qtExtent qtLevel level cellCode
        val           = leafValue node
        (neigh, isec) = getIntersection ext
        ancestorPath  = commonNeighborAncestor qt neigh path
        (nextNode,nextLevel,_) = head ancestorPath
        nextPath      = traverseToLevel nextNode nextLevel 0 next ++ tail ancestorPath
        --LocCode iseccode = cellLocCode (unsafeQtLocCode qt intersection) level
        LocCode iseccode = unsafeQtLocCode qt isec
        next = LocCode (liftA3 mkNext (unNg neigh) iseccode (unLocCode cellCode))
        mkNext Down _ c = (c-1)
        mkNext Up   _ c = (c+cellSize)
        mkNext Same i _ = i
        cellSize        = bit (unLevel level)

    getIntersection :: Extent v srid -> (Neighbor v, Point v srid)
    getIntersection ext = case catMaybes matches of
                           [] -> error "no matches"
                           m  -> head m
      where matches = map ($ ext) checkers

    toCheck = neighborsToCheck fromV toV
    checkers = map (mkNeighborChecker from to) toCheck
    -}
                        
{-# INLINE traceRay #-}

commonNeighborAncestor
  :: VectorSpace v
  => QuadTree v srid a -> Neighbor v -> [(QNode v srid a, Level, LocCode v)]
  -> [(QNode v srid a, Level, LocCode v)]
commonNeighborAncestor qt ns path
  | isNothing neighborCode = []
  | otherwise              = go path
  where
    go p
      | commonLevel == 0 = error "no pue ser"
      | null p          = []
#if DEBUG
      | traceShow ("go cn", commonLevel, l, map (\(_,l',_)->l') p) False
      = undefined
#endif
      | l < commonLevel = go (tail p)
      | otherwise       = p
      where (_, Level l, _) = head p
    (_,level,code) = head path
    commonLevel    = commonAncestorLevel (LocCode (fromJust neighborCode)) code
    neighborCode   = sequence (liftA2 setComp (unNg ns) (unLocCode code))
    setComp Up   c = if c+cellSize < maxCode then Just (c+cellSize) else Nothing
    setComp Down c = if c/=0                 then Just (c-1)        else Nothing
    setComp Same c = Just c
    cellSize       = bit (unLevel level)
    maxCode        = bit (unLevel (qtLevel qt))

commonAncestorLevel :: VectorSpace v => LocCode v -> LocCode v -> Int
commonAncestorLevel (LocCode a) (LocCode b)
  = F.maximum (fmap componentLevel diff)
  where
    componentLevel d = numBits - countLeadingZeros d
    diff     = liftA2 xor a b
    numBits = finiteBitSize (undefined :: Word)

neighborsToCheck :: VectorSpace v => Vertex v -> Vertex v -> [Neighbor v]
neighborsToCheck from to
  = filter checkNeighbor (sortBy neighborCompare neighbors)
  where
    checkNeighbor (Ng ns) = F.all id (liftA3 checkComp ns from to)
    signs                 = fmap signum (to-from)
    checkComp Same _ _    = True
    checkComp Down from' to' = -epsilon > (to'-from')
    checkComp Up   from' to' = (to'-from') > epsilon
    checkComp _    _     _   = False
    neighborCompare a b
      | not (hasSame a), hasSame b = LT
      | hasSame a, not (hasSame b) = GT
      | otherwise                  = EQ
{-# INLINE neighborsToCheck #-}

epsilon = 1e-12

hasSame :: VectorSpace v => Neighbor v -> Bool
hasSame = F.any (==Same) . unNg
{-# INLINE hasSame #-}

mkNeighborChecker
  :: forall v srid. (VectorSpace v, KnownNat (VsDim v -1), Show (v NeighborDir))
  => Point v srid -> Point v srid -> Neighbor v
  -> (Extent v srid -> Maybe (Neighbor v, Point v srid))
mkNeighborChecker from to ng@(Ng ns) ext@(Extent lo hi)
  = case intersection origin of
      Nothing                      -> error "should not happen"
#if DEBUG
      Just vertex | traceShow ("checkNg: ", ext,ng,vertex,inRange vertex) False -> undefined
#endif
      Just vertex | inRange vertex -> Just (ng, Point vertex)
      _                            -> Nothing
  where
    Point lineOrigin = from
    lineDir          = _pVertex to - lineOrigin
    intersection  = lineHyperplaneIntersection lineDir lineOrigin planeDirs

    origin = liftA3 originComp ns lo hi

    originComp Up _   hi' = hi'
    originComp _  lo' _   = lo'

    nv = toVector (toVectorN ns)

    planeDirs :: V (VsDim v - 1) (Direction v) 
    planeDirs = V $ V.generate numPlanes
                (\j -> fromVectorN (V (V.imap (genPlaneDirComp j) nv)))

    genPlaneDirComp
      | hasSame ng = \_ _ n -> if n==Same then 1 else 0
      | otherwise  = \j i _ -> if j==i    then 1 else 0

    numPlanes = dim (Proxy :: Proxy v) - 1


    inRange v = F.all id (inRangeComp <$> ns <*> lo <*> hi <*> v)

    inRangeComp Same lo' hi' v = lo' <= v && v <= hi'
    inRangeComp Up   _   hi' v = abs (hi'-v) <= epsilon
    inRangeComp Down lo' _   v = abs (v-lo') <= epsilon
    
{-# INLINE mkNeighborChecker #-}
  


innerExtent :: VectorSpace v => Quadrant v -> Extent v srid -> Extent v srid
innerExtent (Q qv) (Extent lo hi) = Extent lo' hi'
  where
    lo'             = liftA3 mkLo qv lo hi
    hi'             = liftA3 mkHi qv lo hi
    mkLo First  l _ = l
    mkLo Second l h = (l+h) / 2
    mkHi First  l h = (l+h) / 2
    mkHi Second _ h = h
{-# INLINE innerExtent #-}


outerExtent :: VectorSpace v => Quadrant v -> Extent v srid -> Extent v srid
outerExtent (Q qv) (Extent lo hi) = Extent lo' hi'
  where
    lo'             = liftA3 mkLo qv lo hi
    hi'             = liftA3 mkHi qv lo hi
    mkLo First  l _ = l
    mkLo Second l h = 2*l - h
    mkHi First  l h = 2*h - l
    mkHi Second _ h = h
{-# INLINE outerExtent #-}
