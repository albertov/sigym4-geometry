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
neighbors = sortBy vertexNeighborsFirst $ do
  n <- V.replicateM (dim (Proxy :: Proxy v)) [minBound..maxBound]
  guard (not (V.all (==Same) n))
  return (Ng (fromVectorN (V n)))
  where
    vertexNeighborsFirst a b
      | not (hasSame a), hasSame b = LT
      | hasSame a, not (hasSame b) = GT
      | otherwise                  = EQ

hasSame :: VectorSpace v => Neighbor v -> Bool
hasSame = F.any (==Same) . unNg
{-# INLINE hasSame #-}


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



newtype LocCode v = LocCode {unLocCode :: v Word}

deriving instance Eq (v Word) => Eq (LocCode v)
deriving instance Show (v Word) => Show (LocCode v)

newtype QtVertex v = QtVertex {unQtVertex :: Vertex v}
deriving instance VectorSpace v => Show (QtVertex v)
deriving instance VectorSpace v => Eq (QtVertex v)


maxValue :: Level -> Word
maxValue (Level l) = 2 ^ l

qtMinBox :: VectorSpace v => QuadTree v srid a -> Vertex v
qtMinBox QuadTree{qtLevel=l, qtExtent=e}
  = fmap (/ (fromIntegral (maxValue l))) (eSize e)


qtBackward :: VectorSpace v => QuadTree v srid a -> Point v srid -> QtVertex v
qtBackward QuadTree{qtExtent=Extent lo hi} (Point v)
  = QtVertex ((v/(hi-lo) - (lo/(hi-lo))))
{-# INLINE qtBackward #-}

qtForward :: VectorSpace v => QuadTree v srid a -> QtVertex v -> Point v srid
qtForward QuadTree{qtExtent=Extent lo hi} (QtVertex v)
  = Point (lo + v*(hi-lo))
{-# INLINE qtForward #-}

qtVertex2LocCode
  :: VectorSpace v => QuadTree v srid a -> QtVertex v -> LocCode v
qtVertex2LocCode qt = LocCode . fmap (truncate . (*m)) . unQtVertex
  where m = fromIntegral (maxValue (qtLevel qt))

qtLocCode2Vertex
  :: VectorSpace v => QuadTree v srid a -> LocCode v -> QtVertex v
qtLocCode2Vertex qt = QtVertex  . fmap ((/m) . fromIntegral) . unLocCode
  where m = fromIntegral (maxValue (qtLevel qt))


validVertex :: VectorSpace v => QtVertex v -> Bool
validVertex = contains (Extent (pure 0) (pure 1)) . Point . unQtVertex

qtLocCode
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe (LocCode v)
qtLocCode qt p
  | validVertex qv = Just (qtVertex2LocCode qt qv)
  | otherwise      = Nothing
  where qv = qtBackward qt p
{-# INLINE qtLocCode #-}

unsafeQtLocCode
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> LocCode v
unsafeQtLocCode qt = qtVertex2LocCode qt . qtBackward qt
{-# INLINE unsafeQtLocCode #-}

calculateExtent
  :: VectorSpace v
  => QuadTree v srid a -> Level -> LocCode v -> Extent v srid
calculateExtent qt level code = Extent lo hi
  where
    Point lo  = qtForward qt (qtLocCode2Vertex qt code)
    Point hi  = qtForward qt (qtLocCode2Vertex qt code')
    code'     = LocCode (fmap (+cellSize) (unLocCode code))
    cellSize  = bit (unLevel level)
{-# INLINE calculateExtent #-}


qtCellCode
  :: VectorSpace v => Level -> LocCode v -> LocCode v
qtCellCode (Level l) code
  | l == 0     = code
  | otherwise  = LocCode (fmap (.&. mask) (unLocCode code))
  where mask = complement (U.sum (U.generate l bit))
{-# INLINE qtCellCode #-}

traverseToLevel
  :: VectorSpace v
  => QNode v srid a -> Level -> Level -> LocCode v
  -> (QNode v srid a, Level, LocCode v)
traverseToLevel node start end code = go (node, start, cellCode start)
  where
    go nl@((QLeaf _),_,_)   = nl
    go nl@(_,l,_) | l<=end  = nl
    go (QNode (V v),l,_) = let n' = v `V.unsafeIndex` ix
                               ix = ixFromLocCode l' code
                               l' = l - 1
                           in go (n',l',cellCode l')
    cellCode = flip qtCellCode code
{-# INLINE traverseToLevel #-}


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
            ext           = calculateExtent qt level cellCode
        in Just (leafValue node, ext)
      Nothing -> Nothing
{-# INLINE lookupByPoint #-}

leafValue :: QNode v srid a -> a
leafValue (QLeaf v) = v
leafValue _         = error "expected a leaf"
{-# INLINE leafValue #-}


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
        ext           = calculateExtent qt level cellCode
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
                           []        -> error "no matches"
                           ((n,v):_) -> (n, Point v)
      where matches = map ($ ext) checkers

    toCheck = neighborsToCheck fromV toV
    checkers = map (mkNeighborChecker fromV toV) toCheck

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
  = filter checkNeighbor neighbors
  where
    checkNeighbor (Ng ns)    = F.all id (liftA3 checkComp ns from to)
    checkComp Same _ _       = True
    checkComp Down from' to' = not (nearZero (to'-from')) && from'> to'
    checkComp Up   from' to' = not (nearZero (to'-from')) && to'  > from'
    checkComp _    _     _   = False
{-# INLINE neighborsToCheck #-}

mkNeighborChecker
  :: forall v srid. (VectorSpace v, KnownNat (VsDim v -1), Show (v NeighborDir))
  => Vertex v -> Vertex v -> Neighbor v
  -> (Extent v srid -> Maybe (Neighbor v, Vertex v))
mkNeighborChecker from to ng@(Ng ns) ext@(Extent lo hi)
  = case intersection origin of
      Nothing                      -> error "should not happen"
#if DEBUG
      Just vertex | traceShow ("checkNg: ", ext,ng,vertex,inRange vertex) False -> undefined
#endif
      Just vertex | inRange vertex -> Just (ng, vertex)
      _                            -> Nothing
  where
    lineDir       = to - from
    intersection  = lineHyperplaneIntersection lineDir from planeDirs

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
    inRangeComp Up   _   hi' v = nearZero (hi'-v)
    inRangeComp Down lo' _   v = nearZero (v-lo')
    
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
