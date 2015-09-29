{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
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

module Sigym4.Geometry.QuadTree.Internal.Algorithms where


import Control.Applicative ((<$>), (<*>), pure, liftA2, liftA3)
import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix(mfix))
import Data.Proxy (Proxy(..))
import Data.Bits
import Language.Haskell.TH.Syntax
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree.Internal.Types

import GHC.TypeLits

#if DEBUG
import Debug.Trace
#endif



generate
  :: (MonadFix m, VectorSpace v)
  => Node m v srid a -> Extent v srid -> Level -> m (QuadTree v srid a)
generate build ext level
  | level > maxBound || level < minBound
  = fail "QuadTree.generate: invalid level"
  | otherwise
  = QuadTree <$> genNode undefined ext level build
             <*> pure ext
             <*> pure level
generate2
  :: (MonadFix m, VectorSpace v)
  => Node m v srid a -> Extent v srid -> Vertex v -> m (QuadTree v srid a)
generate2 build ext minBox = generate build effectiveExt level
  where effectiveExt = Extent (eMin ext) (eMin ext + delta)
        delta  = fmap (* maxVal) minBox 
        maxVal = fromIntegral (maxValue level)
        level  = Level (ceiling (logBase 2 nCells))
        nCells = F.maximum (eSize ext / minBox)


genNode
  :: (MonadFix m, VectorSpace v)
  => QNode v srid a -> Extent v srid -> Level -> Node m v srid a
  -> m (QNode v srid a)
genNode parent _   _ (Leaf v) = return (QLeaf parent v)
genNode parent ext level (Node f)
  | level > 0 = mfix (\node -> genQNode parent $ \q -> do
                        next <- liftM snd (f (innerExtent q ext))
                        genNode node (innerExtent q ext) (level-1) next)
  | otherwise = liftM (QLeaf parent . fst) (f ext)

genQNode
  :: forall m v srid a. (MonadFix m, VectorSpace v)
  => QNode v srid a -> (Quadrant v -> m (QNode v srid a))
  -> m (QNode v srid a)
genQNode parent f = liftM (QNode parent . V) (V.generateM numNodes (f . toEnum))
  where numNodes = 2 ^ dim (Proxy :: Proxy v) 

grow
  :: (MonadFix m, VectorSpace v)
  => Node m v srid a -> Quadrant v -> QuadTree v srid a -> m (QuadTree v srid a)
grow build dir (QuadTree oldRoot ext oldLevel)
  | oldLevel + 1 > maxBound = fail "QuadTree.grow: cannot grow"
  | otherwise
  = QuadTree <$> newRoot <*> pure newExt <*> pure (oldLevel + 1)
  where
    newRoot
      = mfix (\node -> genQNode undefined $ \q ->
              if q == dir
                then (return oldRoot)
                else genNode node (innerExtent q newExt) oldLevel build)
    newExt = outerExtent dir ext


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
{-# INLINE qtVertex2LocCode #-}

qtLocCode2Vertex
  :: VectorSpace v => QuadTree v srid a -> LocCode v -> QtVertex v
qtLocCode2Vertex qt = QtVertex  . fmap ((/m) . fromIntegral) . unLocCode
  where m = fromIntegral (maxValue (qtLevel qt))
{-# INLINE qtLocCode2Vertex #-}


validVertex :: VectorSpace v => QtVertex v -> Bool
validVertex = contains (Extent (pure 0) (pure 1)) . Point . unQtVertex
{-# INLINE validVertex #-}


qtLocCode
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe (LocCode v)
qtLocCode qt p
  | validVertex qv = Just (qtVertex2LocCode qt qv)
  | otherwise      = Nothing
  where qv = qtBackward qt p
{-# INLINE qtLocCode #-}

calculateForwardExtent
  :: VectorSpace v
  => QuadTree v srid a -> Level -> LocCode v -> Extent v srid
calculateForwardExtent qt level code = Extent lo hi
  where
    Extent lo' hi' = calculateExtent qt level code
    Point lo       = qtForward qt (QtVertex lo')
    Point hi       = qtForward qt (QtVertex hi')
{-# INLINE calculateForwardExtent #-}

calculateExtent
  :: VectorSpace v
  => QuadTree v srid a -> Level -> LocCode v -> Extent v 0
calculateExtent qt level code = Extent lo hi
  where
    QtVertex lo = qtLocCode2Vertex qt code
    QtVertex hi = qtLocCode2Vertex qt code'
    code'       = LocCode (fmap (+cellSize) (unLocCode code))
    cellSize    = bit (unLevel level)
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
    go nl@((QLeaf{}),_,_)          = nl
    go nl@(_,l,_) | l<=end         = nl
    go (QNode {qChildren=V v},l,_) = let !n' = v `V.unsafeIndex` ix
                                         !ix = ixFromLocCode l' code
                                         !l' = l - 1
                                     in go (n',l',cellCode l')
    cellCode = flip qtCellCode code
{-# INLINE traverseToLevel #-}


ixFromLocCode
  :: VectorSpace v
  => Level -> LocCode v -> Int
ixFromLocCode (Level l) = snd . F.foldl' go (0::Int,0) . unLocCode
  where go (!i,!s) v = (i+1, s + (fromEnum (v `testBit` l) `unsafeShiftL` i))
{-# INLINE ixFromLocCode #-}

lookupByPoint
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe (a, Extent v srid)
lookupByPoint qt@QuadTree{..} p
  = case qtLocCode qt p of
      Just c ->
        let (node,level,cellCode) = traverseToLevel qtRoot qtLevel 0 c
            ext           = calculateForwardExtent qt level cellCode
        in Just (leafData node, ext)
      Nothing -> Nothing
{-# INLINE lookupByPoint #-}


leafData :: QNode v srid a -> a
leafData QLeaf{..} = qData
leafData _         = error "expected a leaf"
{-# INLINE leafData #-}


traceRay :: forall v srid a. (VectorSpace v, KnownNat (VsDim v - 1), Eq (v Word)
#if DEBUG
  , Show a, Show (v Word), Show (v NeighborDir)
#endif
  )
  => QuadTree v srid a -> Point v srid -> Point v srid -> [a]
traceRay qt@QuadTree{..} from to
  | almostEqVertex (unQtVertex fromV) (unQtVertex toV) = []
  | not (validVertex fromV) = []
  | not (validVertex toV)   = []
#if DEBUG
  | traceShow ("traceRay from:",qtExtent,qtLevel,from,to) False = undefined
#endif
  | otherwise  = go (qtVertex2LocCode qt fromV) qtRoot qtLevel
  where
    go !code !startNode !startLevel
#if DEBUG
      | traceShow ("go", code) False = undefined
#endif
#if ASSERTS
      | not (qtValidCode qt code) = error "traceRay: reached bad vertex"
      | next     == code          = error "traceRay: got in a loop"
#endif
      | cellCode == cellCodeTo = [val]
      | otherwise              = val:(go next ancestor ancestorLevel)
      where
        (node, level, cellCode) = traverseToLevel startNode startLevel 0 code
        cellCodeTo    = qtCellCode level codeTo 
        ext           = calculateExtent qt level cellCode
        val           = leafData node
        (neigh, isec) = getIntersection ext
        LocCode iseccode = qtVertex2LocCode qt isec
        next = LocCode (liftA3 mkNext (ngDirection neigh) iseccode (unLocCode cellCode))
        ancestorLevel = commonAncestorLevel code next
        ancestor      = findAncestor node level

        findAncestor !n !l
          | l==ancestorLevel = n
          | otherwise        = findAncestor (qParent n) (l+1)

        mkNext Down _ c = c - 1
        mkNext Up   _ c = c + cellSize
        mkNext Same i _ = i
        cellSize        = bit (unLevel level)

    fromV  = qtBackward qt from
    toV    = qtBackward qt to
    codeTo = qtVertex2LocCode qt toV

    getIntersection :: Extent v 0 -> (Neighbor v, QtVertex v)
    getIntersection ext = head
                        . checkNotEmpty
                        . map snd
                        . filter fst
                        . map (\n -> neighborIntersection fromV toV n ext)
                        . filter (checkNeighbor fromV toV)
                        $ neighbors
#if ASSERTS
    qtValidCode QuadTree{qtLevel=l} = F.all (<maxValue l) . unLocCode
    checkNotEmpty l
      | null l    = error "no neighbors intersects"
      | otherwise = l
#else
    checkNotEmpty = id
#endif

{-# SPECIALISE[2] traceRay ::
      QuadTree V2 srid a -> Point V2 srid -> Point V2 srid -> [a] #-}
{-# SPECIALISE[2] traceRay ::
      QuadTree V3 srid a -> Point V3 srid -> Point V3 srid -> [a] #-}
{-# SPECIALISE[2] traceRay ::
      QuadTree V4 srid a -> Point V4 srid -> Point V4 srid -> [a] #-}
      

commonAncestorLevel :: VectorSpace v => LocCode v -> LocCode v -> Level
commonAncestorLevel (LocCode a) (LocCode b)
  = Level (F.maximum (fmap componentLevel diff))
  where
    componentLevel d = maxLevel - countLeadingZeros d + 1
    diff             = liftA2 xor a b
    Level maxLevel   = maxBound
{-# INLINE commonAncestorLevel #-}


checkNeighbor (QtVertex from) (QtVertex to) ng
  = F.all id (liftA3 checkComp (ngDirection ng) from to) 
  where
    checkComp Same _ _       = True
    checkComp Down from' to' = not (nearZero (to'-from')) && from'> to'
    checkComp Up   from' to' = not (nearZero (to'-from')) && to'  > from'
{-# INLINE checkNeighbor #-}

neighborIntersection
  :: forall v srid. (VectorSpace v, KnownNat (VsDim v -1)
#if DEBUG
  , Show (v NeighborDir)
#endif
  )
  => QtVertex v -> QtVertex v
  -> Neighbor v -> Extent v srid -> (Bool, (Neighbor v, QtVertex v))
neighborIntersection (QtVertex from) (QtVertex to) ng (Extent lo hi)
#if DEBUG
  | traceShow ("checkNg: ", ng,origin,lineDir,planeDirs) False = undefined
#endif
  | inRange vertex = (True, (ng, QtVertex vertex))
  | otherwise      = (False, (ng, QtVertex vertex))
  where
#if ASSERTS
    vertex  = maybe
                (error "The impossible! no intersection found with neighbor")
                id
                (lineHyperplaneMaybeIntersection lineDir from planeDirs origin)
#else
    vertex  = lineHyperplaneIntersection lineDir from planeDirs origin
#endif

    lineDir = to - from

    origin  = liftA3 go (ngDirection ng) lo hi
      where
        go Up   _   hi' = hi'
        go Same lo' _   = lo'
        go Down lo' _   = lo'-epsilon

    epsilon = 1e-12

    planeDirs = ngNormals ng


    inRange vx = F.all id (go <$> (ngDirection ng) <*> lo <*> hi <*> vx)
      where
        go Same lo' hi' v = (nearZero (v-lo') || lo' < v) && v < hi'
        go Down lo' _   v = nearZero (v-lo'+epsilon)
        go Up   _   hi' v = nearZero (hi'-v)
    
{-# INLINE neighborIntersection #-}
  


innerExtent :: VectorSpace v => Quadrant v -> Extent v srid -> Extent v srid
innerExtent (Quadrant qv) (Extent lo hi) = Extent lo' hi'
  where
    lo'             = liftA3 mkLo qv lo hi
    hi'             = liftA3 mkHi qv lo hi
    mkLo First  l _ = l
    mkLo Second l h = (l+h) / 2
    mkHi First  l h = (l+h) / 2
    mkHi Second _ h = h
{-# INLINE innerExtent #-}


outerExtent :: VectorSpace v => Quadrant v -> Extent v srid -> Extent v srid
outerExtent (Quadrant qv) (Extent lo hi) = Extent lo' hi'
  where
    lo'             = liftA3 mkLo qv lo hi
    hi'             = liftA3 mkHi qv lo hi
    mkLo First  l _ = l
    mkLo Second l h = 2*l - h
    mkHi First  l h = 2*h - l
    mkHi Second _ h = h
{-# INLINE outerExtent #-}

neighbors :: forall v. (VectorSpace v, KnownNat (VsDim v - 1)) => Neighbors v
neighbors = neighborsDefault
{-# NOINLINE neighbors #-}


neighborsV2 :: Neighbors V2
neighborsV2 = $$(mkNeighbors)
{-# INLINE[2] neighborsV2 #-}
{-# RULES "neighbors/V2"[2] neighbors = neighborsV2 #-}

neighborsV3 :: Neighbors V3
neighborsV3 = $$(mkNeighbors)
{-# INLINE[2] neighborsV3 #-}
{-# RULES "neighbors/V3"[2] neighbors = neighborsV3 #-}

neighborsV4 :: Neighbors V4
neighborsV4 = $$(mkNeighbors)
{-# INLINE[2] neighborsV4 #-}
{-# RULES "neighbors/V4"[2] neighbors = neighborsV4 #-}

