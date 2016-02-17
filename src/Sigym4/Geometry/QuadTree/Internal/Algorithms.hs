{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE ViewPatterns #-}

module Sigym4.Geometry.QuadTree.Internal.Algorithms (
    traceRay
  , traceRay2
  , lookupByPoint
  , qtContainsPoint
  , grow
  , growToInclude
) where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Fix (MonadFix(mfix))
import Data.Strict.Maybe (Maybe(..), isJust, isNothing, fromJust)
import Data.Strict.Tuple (Pair ((:!:)))
import Data.Proxy (Proxy(..))
import Data.Bits

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree.Internal.Types

import Prelude hiding (Maybe(..), maybe)
import qualified Prelude as P


-- Translates a Point's coordinates to a Vertex in the [0,1) Extent. Makes sure
-- to round the coordinates to the highest resolution we can safely calculate
-- intersections with traceRay
qtBackward :: VectorSpace v => QuadTree v srid a -> Point v srid -> QtVertex v
qtBackward (qtExtent -> Extent lo hi) (Point v)
  = QtVertex $ (fmap ((/absMax) . trunc . (*absMax)) ratio)
  where !ratio   = (v/(hi-lo) - (lo/(hi-lo)))
        !trunc   = (fromIntegral :: Int -> Double) . truncate
        !boxBits = qtEpsLevel - nearZeroBits - 1
        !absMax  = fromIntegral (maxValue (Level boxBits))
{-# INLINE qtBackward #-}

qtForward :: VectorSpace v => QuadTree v srid a -> QtVertex v -> Point v srid
qtForward (qtExtent -> Extent lo hi) (QtVertex v)
  = Point (lo + v*(hi-lo))
{-# INLINE qtForward #-}

vertex2LocCode
  :: VectorSpace v => Level -> QtVertex v -> LocCode v
vertex2LocCode l (QtVertex v)  = LocCode (fmap (truncate . (*m)) v)
  where !m = fromIntegral (maxValue l)
{-# INLINE vertex2LocCode #-}

qtVertex2LocCode
  :: VectorSpace v => QuadTree v srid a -> QtVertex v -> LocCode v
qtVertex2LocCode qt = vertex2LocCode (qtLevel qt)
{-# INLINE qtVertex2LocCode #-}

locCode2Vertex
  :: VectorSpace v => Level -> LocCode v -> QtVertex v
locCode2Vertex l (LocCode c) = QtVertex (fmap ((/m) . fromIntegral) c)
  where !m = fromIntegral (maxValue l)
{-# INLINE locCode2Vertex #-}

qtLocCode2Vertex
  :: VectorSpace v => QuadTree v srid a -> LocCode v -> QtVertex v
qtLocCode2Vertex qt = locCode2Vertex (qtLevel qt)
{-# INLINE qtLocCode2Vertex #-}


qtContainsPoint :: VectorSpace v => QuadTree v srid a -> Point v srid -> Bool
qtContainsPoint qt = isJust . qtLocCode qt
{-# INLINE qtContainsPoint #-}

qtLocCode
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe (LocCode v)
qtLocCode qt p
  | all (\c -> 0<=c && c<1) (unQtVertex p') = Just code
  | otherwise                               = Nothing
  where
    !p'   = qtBackward qt p
    !code = qtVertex2LocCode qt p'
{-# INLINE qtLocCode #-}

calculateExtent
  :: VectorSpace v
  => QuadTree v srid a -> Level -> LocCode v -> Extent v NoCrs
calculateExtent qt l code = Extent lo hi
  where
    !(QtVertex lo) = qtLocCode2Vertex qt code
    !(QtVertex hi) = qtLocCode2Vertex qt code'
    !code'         = LocCode (fmap (+(maxValue l)) (unLocCode code))
{-# INLINE calculateExtent #-}


qtCellCode
  :: VectorSpace v => Level -> LocCode v -> LocCode v
qtCellCode l (LocCode code) = LocCode (fmap (.&. mask) code)
  where !mask = complement $! maxValue l - 1
{-# INLINE qtCellCode #-}

traverseToLevel
  :: VectorSpace v
  => TraversedNode v srid a
  -> Level -> LocCode v -> TraversedNode v srid a
traverseToLevel TNode{tNode=node, tLevel=start} end code = go node start
  where
    go !n@QLeaf{} !l          = TNode l n (qtCellCode l code)
    go !n         !l | l<=end = TNode l n (qtCellCode l code)
    go !QNode{qChildren=c} !l = let n' = getChildAtLevel c l' code
                                    l' = Level (unLevel l - 1)
                                in go n' l'
{-# INLINE traverseToLevel #-}

qtTraverseToLevel
  :: VectorSpace v
  => QuadTree v srid a
  -> Level -> LocCode v -> TraversedNode v srid a
qtTraverseToLevel QuadTree{..}
  = traverseToLevel (TNode qtLevel qtRoot (LocCode (pure 0)))
{-# INLINE qtTraverseToLevel #-}




lookupByPoint
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> P.Maybe a
lookupByPoint qt p
  = case qtLocCode qt p of
      Just c ->
        let TNode{tNode=node} = qtTraverseToLevel qt minBound c
        in P.Just (leafData node)
      Nothing -> P.Nothing
{-# INLINE lookupByPoint #-}


leafData :: QNode v srid a -> a
leafData QLeaf{..} = qData
leafData _         = error "expected a leaf"
{-# INLINE leafData #-}

leavesTouching
  :: forall v srid a. VectorSpace v
  => NeighborPosition v -> TraversedNode v srid a -> [TraversedNode v srid a]
leavesTouching pos = go
  where
    go :: TraversedNode v srid a -> [TraversedNode v srid a]
    go !n@TNode{tNode=QLeaf{}} = [n]
    go !TNode{tNode=QNode{qChildren=cs}, tLevel=Level l, tCellCode=code}
      = concatMap go (map getChild' (quadrantsTouching pos))
      where getChild' q = TNode { tNode     = getChild cs q
                                , tLevel    = Level (l-1)
                                , tCellCode = setChildBits (Level (l-1)) q code}


quadrantsTouching :: VectorSpace v => NeighborPosition v -> [Quadrant v]
quadrantsTouching pos
  = filter (all id . liftA2 match pos . unQuadrant) [minBound..maxBound]
  where
    match Same _      = True
    match Up   Second = True
    match Down First  = True
    match _    _      = False

traceRay :: forall v srid a. (HasHyperplanes v, Eq (v Int), Num (v Int))
  => QuadTree v srid a -> Point v srid -> Point v srid -> [a]
traceRay qt from = map snd . traceRay2 qt from

traceRay2 :: forall v srid a. (HasHyperplanes v, Eq (v Int), Num (v Int))
  => QuadTree v srid a -> Point v srid -> Point v srid
  -> [(Point v srid, a)]
traceRay2 qt@QuadTree{..} from to
  | isJust mCodeFrom && isJust mCodeTo =
      go [(from,tNodeFrom)] maxIterations
  | otherwise = []
  where
    -- maxIterations makes sure we don't end up in an infinite loop if there
    -- is a problem with the implementation. We absolutely shouldn't cross
    -- more cells than the number of smallest possible cells along the borders
    -- of the qtree (less in fact but this serves our purposes)
    maxIterations = 2 ^ (unLevel qtLevel + dim (Proxy :: Proxy v)) :: Int

    -- If there are no more candidates to continue tracing or we reached the
    -- maximum number of intersections then there's a bug. If in production
    -- stop the tracing and hope for the best.
#if ASSERTS
    go [] !_ = error "no intersections"
    go _  !0 = error "iteration limit reached"
#else
    go [] !_ = []
    go _  !0 = []
#endif

    go (!(isec,cur):rest) !n
      -- We reached our destination, stop the tracing
      | tCellCode cur == cellCodeTo = [value]

      -- There are no valid intersections in the current cell, try with the
      -- alternatives
      | null next                   = go rest n

      -- There's at least one intersection out of this cell, go to the next
      -- cell
      | otherwise                   = value : go next (n-1)
      where
        -- The next cell candidates. the "fuzzy" ones first or we risk
        -- sticking into a loop. We also try non-fuzzy intersections in case
        -- the intersection is very near the extent corners and we've missed it
        next       = catMaybes
                   . map mkNext
                   . catMaybes
                   $ getIntersections fuzzyExt ++ getIntersections cellExt

        cellCodeTo = qtCellCode (tLevel cur) codeTo

        value      = (isec,leafData (tNode cur))

        cellExt    = calculateExtent qt (tLevel cur) (tCellCode cur)

        -- We try intersection with a fuzzy extent which is slightly larger
        -- than the current cell's one so we end up in the correct neighbor in
        -- case the intersection is just in the middle of two smaller sized
        -- neighbors
        fuzzyExt   = Extent (fmap (subtract qtEpsilon) (eMin cellExt))
                            (fmap (+ qtEpsilon)        (eMax cellExt))

        getIntersections ext
          = map (neighborIntersection ext)
          . filter (checkNeighbor fromV toV)
          $ neighbors

        -- Calculates the LocCode of the intersection with a neighbor.
        -- If the intersection is very close to our destination the we calculate
        -- it normally. Else we add our cell's width to our cell code or
        -- subtract 1 to it
        mkNext !(isec :!: pos)
          | all qtNearZero (unQtVertex isec - unQtVertex toV)
          = Just (isecPt, traverseViaAncestor qt cur minBound codeTo)
          | any isNothing mNext = Nothing
          | otherwise = Just (isecPt, traverseViaAncestor qt cur minBound next')
          where
            isecPt = qtForward qt isec
            mNext = liftA3 mkNextCode pos (unLocCode (qtVertex2LocCode qt isec))
                                          (unLocCode (tCellCode cur))
            next' = LocCode (fmap fromJust mNext)

            cellSize = maxValue (tLevel cur)

            mkNextCode Same i _                 = Just i
            mkNextCode Down _ c
              | c > 0                           = Just (c - 1)
              | otherwise                       = Nothing
            mkNextCode Up   _ c
              | c + cellSize < maxValue qtLevel = Just (c + cellSize)
              | otherwise                       = Nothing

    fromV     = qtBackward qt from
    toV       = qtBackward qt to
    lineDir   = toV - fromV
    mCodeTo   = qtLocCode qt to
    mCodeFrom = qtLocCode qt from
    codeTo    = fromJust mCodeTo
    codeFrom  = fromJust mCodeFrom
    tNodeFrom = qtTraverseToLevel qt minBound codeFrom

    -- Calculates the QtVertex of the possible intersection with a given
    -- neighbor. It is a valid intersection only if it is within the edge's
    -- bounds and hasn't overshooted our destination.
    neighborIntersection (Extent lo hi) ng
      | isValid   = Just (QtVertex vx :!: ngPosition ng)
      | otherwise = Nothing
      where
        isValid = inRange vx && inRayBounds vx

        vx = lineHyperplaneIntersection (ngPlanes ng)
                    (unQtVertex lineDir) (unQtVertex fromV) origin

        origin = liftA3 origin' (ngPosition ng) lo hi
          where
            origin' Up   _   hi'= hi'
            origin' Same lo' _  = lo'
            origin' Down lo' _  = lo'

        inRange v = all id (fmap inRange' (ngPosition ng) <*> lo <*> hi <*> v)
          where
            inRange' Same lo' hi' = qtBetween  lo' hi'
            inRange' Down lo' _   = qtAlmostEq lo'
            inRange' Up   _   hi' = qtAlmostEq hi'

    inRayBounds v = all id (liftA3 qtBetweenC lo hi v)
      where
        !lo = liftA2 min (unQtVertex fromV) (unQtVertex toV)
        !hi = liftA2 max (unQtVertex fromV) (unQtVertex toV)


{-# INLINABLE traceRay2 #-}
{-# SPECIALISE traceRay2 ::
      QuadTree V2 srid a -> Point V2 srid -> Point V2 srid
                         -> [(Point V2 srid, a)] #-}
{-# SPECIALISE traceRay2 ::
      QuadTree V3 srid a -> Point V3 srid -> Point V3 srid
                         -> [(Point V3 srid, a)] #-}
{-# SPECIALISE traceRay2 ::
      QuadTree V4 srid a -> Point V4 srid -> Point V4 srid
                         -> [(Point V4 srid, a)] #-}

-- Calculates whether we need to check an intersection with a given neighbor
-- given the origin and destination of a ray. For example, we don't need to
-- check our top and bottom neighbors if the ray goes exactly from left to right
checkNeighbor :: VectorSpace v => QtVertex v -> QtVertex v -> Neighbor v -> Bool
checkNeighbor (QtVertex from) (QtVertex to) ng
  = all id (liftA3 checkComp (ngPosition ng) from to)
  where
    checkComp Same _ _       = True
    checkComp Down from' to' = from' > to'
    checkComp Up   from' to' = to'   > from'
{-# INLINE checkNeighbor #-}

traverseViaAncestor
  :: VectorSpace v
  => QuadTree v srid a -> TraversedNode v srid a -> Level -> LocCode v
  -> TraversedNode v srid a
traverseViaAncestor qt node level code
  = traverseToLevel (traverseToCommonAncestor qt node code) level code
{-# INLINE traverseViaAncestor #-}

traverseToCommonAncestor
  :: VectorSpace v
  => QuadTree v srid a -> TraversedNode v srid a -> LocCode v
  -> TraversedNode v srid a
traverseToCommonAncestor QuadTree{..} TNode{..} code
  = TNode { tNode     = findAncestor tNode tLevel
          , tLevel    = al
          , tCellCode = qtCellCode al tCellCode
          }
  where
    al = commonAncestorLevel tCellCode code
    findAncestor !n !l
      | l==al     = n
      | otherwise = findAncestor (qParent n) (Level (unLevel l + 1))
{-# INLINE traverseToCommonAncestor #-}


commonAncestorLevel :: VectorSpace v => LocCode v -> LocCode v -> Level
commonAncestorLevel (LocCode a) (LocCode b)
  = Level (maximum (fmap componentLevel diff))
  where
    componentLevel d = finiteBitSize (undefined :: Int) - countLeadingZeros d
    diff             = liftA2 xor a b
{-# INLINE commonAncestorLevel #-}


grow
  :: (MonadFix m, VectorSpace v)
  => Node m v crs a -> Quadrant v -> QuadTree v crs a
  -> m (Either QtError (QuadTree v crs a))
grow build dir (QuadTree oldRoot oldCenter minRadius oldLevel)
  | newLevel > maxBound = return (Left QtCannotGrow)
  | otherwise
  = Right <$> (QuadTree <$> newRoot <*> pure newCenter
                        <*> pure minRadius <*> pure newLevel)
  where
    newCenter   = parentCenter minRadius oldCenter oldLevel dir
    newLevel = Level (unLevel oldLevel + 1)
    newRoot  = mfix $ \node -> genQNode rootParent $ \q ->
        if q == dir
          then updateOldTree node oldRoot
          else do genNode minRadius node
                    (childCenter minRadius newCenter oldLevel q) oldLevel build

    updateOldTree parent (QNode _ cs) = mfix $ \node ->
      genQNode parent $ updateOldTree node . getChild cs
    updateOldTree parent (QLeaf _ v) = return (QLeaf parent v)
{-# INLINABLE grow #-}


growToInclude
  :: (MonadFix m, VectorSpace v)
  => Node m v crs a -> Point v crs -> QuadTree v crs a
  -> m (Either QtError (QuadTree v crs a))
growToInclude build p@(Point vx) = go
  where
    go qt | qt `qtContainsPoint` p = return (Right qt)
    go qt = either (return . Left) go =<< grow build (findQ (qtCenter qt)) qt
    findQ (Point c) = Quadrant (liftA2 findHalf vx c)
    findHalf v c
      | v > c       = First
      | otherwise   = Second
{-# INLINABLE growToInclude #-}


catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust
{-# INLINE catMaybes #-}


neighbors :: HasHyperplanes v => Neighbors v
neighbors = neighborsDefault
{-# NOINLINE neighbors #-}

neighborsV2 :: Neighbors V2
neighborsV2 = $$(mkNeighbors)
{-# INLINE neighborsV2 #-}
{-# RULES "neighbors/V2" neighbors = neighborsV2 #-}

neighborsV3 :: Neighbors V3
neighborsV3 = $$(mkNeighbors)
{-# INLINE neighborsV3 #-}
{-# RULES "neighbors/V3" neighbors = neighborsV3 #-}

neighborsV4 :: Neighbors V4
neighborsV4 = $$(mkNeighbors)
{-# INLINE neighborsV4 #-}
{-# RULES "neighbors/V4" neighbors = neighborsV4 #-}


qtEpsLevel :: Int
qtEpsLevel = fst ($$(machineEpsilonAndLevel 1) :: (Int, Double))
{-# INLINE qtEpsLevel #-}

qtEpsilon  :: Double
qtEpsilon = snd $$(machineEpsilonAndLevel 1)
{-# INLINE qtEpsilon #-}

nearZeroBits :: Int
nearZeroBits = finiteBitSize (undefined :: Int) `div` 16
{-# INLINE nearZeroBits #-}

qtNearZero :: Double -> Bool
qtNearZero a = abs a <= qtEpsilon * 2^nearZeroBits
{-# INLINE qtNearZero #-}

qtAlmostEq :: Double -> Double -> Bool
qtAlmostEq a b = qtNearZero (a-b)
{-# INLINE qtAlmostEq #-}

qtBetween :: Double -> Double -> Double -> Bool
qtBetween lo hi v = (lo < v || qtAlmostEq v lo) && v < hi
{-# INLINE qtBetween #-}

qtBetweenC :: Double -> Double -> Double -> Bool
qtBetweenC lo hi v = (lo < v  || qtAlmostEq v lo) &&
                     (v  < hi || qtAlmostEq v hi)
{-# INLINE qtBetweenC #-}
