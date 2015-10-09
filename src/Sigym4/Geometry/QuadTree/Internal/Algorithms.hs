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

module Sigym4.Geometry.QuadTree.Internal.Algorithms where


import Control.Applicative ((<$>), (<*>), pure, liftA2, liftA3)
import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix(mfix))
import Data.Maybe (isJust, fromMaybe, catMaybes, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Bits

import Sigym4.Geometry
import Sigym4.Geometry.Algorithms
import Sigym4.Geometry.QuadTree.Internal.Types


#if DEBUG
import Debug.Trace (traceShow, trace)
traceShowMsg msg a = traceShow (msg, a) a
#endif

data QtError
  = QtInvalidLevel
  | QtCannotGrow
  deriving (Show, Eq, Enum)


generate
  :: (MonadFix m, VectorSpace v)
  => Node m v srid a -> Extent v srid -> Level
  -> m (Either QtError (QuadTree v srid a))
generate build ext level
  | level > maxBound || level < minBound = return (Left QtInvalidLevel)
  | otherwise
  = Right <$> (QuadTree <$> genNode rootParent ext level build
                        <*> pure ext
                        <*> pure level)

generate2
  :: (MonadFix m, VectorSpace v)
  => Node m v srid a -> Extent v srid -> Box v
  -> m (Either QtError (QuadTree v srid a))
generate2 build ext minBox = generate build effectiveExt level
  where effectiveExt = Extent (eMin ext) (eMin ext + delta)
        delta  = fmap (* maxVal) minBox 
        maxVal = fromIntegral (maxValue level)
        level  = Level (ceiling (logBase 2 nCells))
        nCells = maximum (eSize ext / minBox)


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
genQNode parent f = liftM (QNode parent) (generateChildren (f . toEnum))

grow
  :: (MonadFix m, VectorSpace v)
  => Node m v srid a -> Quadrant v -> QuadTree v srid a
  -> m (Either QtError (QuadTree v srid a))
grow build dir (QuadTree oldRoot ext oldLevel)
  | oldLevel + 1 > maxBound = return (Left QtCannotGrow)
  | otherwise
  = Right <$> (QuadTree <$> newRoot <*> pure newExt <*> pure (oldLevel + 1))
  where
    newRoot
      = mfix (\node -> genQNode rootParent $ \q ->
              if q == dir
                then (return oldRoot {qParent=node})
                else genNode node (innerExtent q newExt) oldLevel build)
    newExt = outerExtent dir ext

setChildBits:: VectorSpace v => Level -> Quadrant v -> LocCode v -> LocCode v
setChildBits (Level l) (Quadrant q) (LocCode code) 
  = LocCode (liftA2 (.|.) code val)
  where
    val = fmap (\c -> case c of {Second->bit l; First->0}) q
{-# INLINE setChildBits #-}

maxValue :: Level -> Word
maxValue (Level l) = bit l
{-# INLINE maxValue #-}

qtMinBox :: VectorSpace v => QuadTree v srid a -> Box v
qtMinBox QuadTree{qtLevel=l, qtExtent=e} = calculateMinBox e l

calculateMinBox :: VectorSpace v => Extent v srid -> Level -> Box v
calculateMinBox e l
  = fmap (/ (fromIntegral (maxValue l))) (eSize e)
{-# INLINE calculateMinBox #-}


qtBackward :: VectorSpace v => QuadTree v srid a -> Point v srid -> QtVertex v
qtBackward QuadTree{qtExtent=Extent lo hi} (Point v)
  = QtVertex $ (fmap ((/absMax) . trunc . (*absMax)) ratio)
  where ratio   = (v/(hi-lo) - (lo/(hi-lo)))
        trunc = (fromIntegral :: Int -> Double) . truncate
        boxBits = qtEpsLevel - nearZeroBits - 1
        absMax  = fromIntegral (maxValue (Level boxBits))
{-# INLINE qtBackward #-}

qtForward :: VectorSpace v => QuadTree v srid a -> QtVertex v -> Point v srid
qtForward QuadTree{qtExtent=Extent lo hi} (QtVertex v)
  = Point (lo + v*(hi-lo))
{-# INLINE qtForward #-}

vertex2LocCode
  :: VectorSpace v => Level -> QtVertex v -> LocCode v
vertex2LocCode l = LocCode . fmap (truncate . (*m)) . unQtVertex
  where m = fromIntegral (maxValue l)
{-# INLINE vertex2LocCode #-}

qtVertex2LocCode
  :: VectorSpace v => QuadTree v srid a -> QtVertex v -> LocCode v
qtVertex2LocCode qt = vertex2LocCode (qtLevel qt)
{-# INLINE qtVertex2LocCode #-}

locCode2Vertex
  :: VectorSpace v => Level -> LocCode v -> QtVertex v
locCode2Vertex l = QtVertex  . fmap ((/m) . fromIntegral) . unLocCode
  where m = fromIntegral (maxValue l)
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
  | insideExt
#if ASSERTS
  , all (\c -> 0<=c && c<1) (unQtVertex p')
#endif
  = Just code
  | otherwise = Nothing
  where
    code      = qtVertex2LocCode qt p'
    p'        = qtBackward qt p
    insideExt = qtExtent qt `contains` p
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
  where mask = complement (bit l - 1)
{-# INLINE qtCellCode #-}

traverseToLevel
  :: VectorSpace v
  => TraversedNode v srid a
  -> Level -> LocCode v -> TraversedNode v srid a
traverseToLevel TNode{tNode=node, tLevel=start} end code = go node start
  where
    go !n@QLeaf{} !l          = TNode l n (qtCellCode l code)
    go !n         !l | l<=end = TNode l n (qtCellCode l code)
    go !QNode{qChildren=c} !l = let n' = getChild c (quadrantAtLevel l' code)
                                    l' = l - 1
                                in go n' l'
{-# INLINE traverseToLevel #-}

qtTraverseToLevel
  :: VectorSpace v
  => QuadTree v srid a
  -> Level -> LocCode v -> TraversedNode v srid a
qtTraverseToLevel QuadTree{..}
  = traverseToLevel (TNode qtLevel qtRoot (LocCode (pure 0)))
{-# INLINE qtTraverseToLevel #-}


data TraversedNode v srid a
  = TNode
    { tLevel    :: Level
    , tNode     :: QNode v srid a
    , tCellCode :: LocCode v
    }

instance Eq (LocCode v) => Eq (TraversedNode v srid a) where
  a == b = tCellCode a == tCellCode b && tLevel a == tLevel b

instance Show (LocCode v) => Show (TraversedNode v srid a) where
  show TNode{..} = concat (["TNode { tLevel = ", show tLevel
                           ,      ", tCellCode = ", show tCellCode, " }"
                           ]) 

quadrantAtLevel :: VectorSpace v => Level -> LocCode v -> Quadrant v
quadrantAtLevel (Level l) = Quadrant . fmap toHalf . unLocCode
  where toHalf v = if v `testBit` l then Second else First
{-# INLINE quadrantAtLevel #-}


lookupByPoint
  :: VectorSpace v
  => QuadTree v srid a -> Point v srid -> Maybe a
lookupByPoint qt p
  = case qtLocCode qt p of
      Just c ->
        let TNode{tNode=node} = qtTraverseToLevel qt 0 c
        in Just (leafData node)
      Nothing -> Nothing
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
    go !TNode{tNode=QNode{qChildren=cs}, tLevel=l, tCellCode=code}
      = concatMap go (map getChild' (quadrantsTouching pos))
      where getChild' q = TNode { tNode     = getChild cs q
                                , tLevel    = l-1
                                , tCellCode = setChildBits (l-1) q code}

quadrantsTouching :: VectorSpace v => NeighborPosition v -> [Quadrant v]
quadrantsTouching pos
  = filter (all id . liftA2 match pos . unQuadrant) [minBound..maxBound]
  where
    match Same _      = True
    match Up   Second = True
    match Down First  = True
    match _    _      = False



traceRay :: forall v srid a. (HasHyperplanes v, Eq (v Word), Num (v Word)
#if DEBUG
  , Show a, Show (v Word), Show (v NeighborDir), Show (VPlanes v (Direction v))
#endif
  )
  => QuadTree v srid a -> Point v srid -> Point v srid -> [a]
traceRay qt@QuadTree{..} from to
#if DEBUG
  | traceShow ( "traceRay"
              , ("from",      fromV)
              , ("to",        toV)
              , ("mCodeFrom", mCodeFrom)
              , ("mCodeTo",   mCodeTo)
              ) False = undefined
#endif
  | isJust (mCodeFrom >> mCodeTo)  = go [tNodeFrom] maxIterations
  | otherwise                      = []
  where
    maxIterations = 2 ^ (unLevel qtLevel + dim (Proxy :: Proxy v)) :: Int
#if ASSERTS
    go [] !_ = error "no intersections"
    go _  !0 = error "iteration limit reached"
#else
    go [] !_ = []
    go _  !0 = []
#endif

    go (!cur:rest) !n
#if DEBUG
      | traceShow ("go", n, tCellCode cur, tLevel cur
                  , cellExt, null next) False = undefined
#endif
      | tCellCode cur == cellCodeTo = [value]
      | null next                   = go rest n
      | otherwise                   = value : go next (n-1)
      where
        next       = catMaybes $ map (>>=mkNext) $
                       getIntersections fuzzyExt ++ getIntersections cellExt

        cellCodeTo = qtCellCode (tLevel cur) codeTo 

        value      = leafData (tNode cur)

        cellExt    = calculateExtent qt (tLevel cur) (tCellCode cur)

        fuzzyExt   = Extent (fmap (subtract qtEpsilon) (eMin cellExt))
                            (fmap (+ qtEpsilon)        (eMax cellExt))

        getIntersections ext
          = map (neighborIntersection ext)
          . filter (checkNeighbor fromV toV)
          $ neighbors

        mkNext !(!isec, pos) = do
          nCode <- if all qtNearZero (unQtVertex isec - unQtVertex toV)
                     then Just codeTo
                     else liftM LocCode $ sequence $ liftA3
                               mkNextCode pos
                                          (unLocCode (qtVertex2LocCode qt isec))
                                          (unLocCode (tCellCode cur))
          ancestor <- traverseToCommonAncestor qt cur nCode
          return (traverseToLevel ancestor 0 nCode)

        mkNextCode Down _ c
          | c > 0                           = Just (c - 1)
          | otherwise                       = Nothing
        mkNextCode Up   _ c
          | c + cellSize < maxValue qtLevel = Just (c + cellSize)
          | otherwise                       = Nothing
        mkNextCode Same i _                 = Just i

        cellSize = maxValue (tLevel cur)

    fromV     = qtBackward qt from
    toV       = qtBackward qt to
    lineDir   = toV - fromV
    mCodeTo   = qtLocCode qt to
    mCodeFrom = qtLocCode qt from
    codeTo    = fromMaybe (error "traceRay: invalid use of codeTo")   mCodeTo
    codeFrom  = fromMaybe (error "traceRay: invalid use of codeFrom") mCodeFrom
    tNodeFrom = qtTraverseToLevel qt 0 codeFrom

    neighborIntersection (Extent lo hi) ng
      = fmap (\v -> (QtVertex v, ngPosition ng))
      . listToMaybe . filter isValid . map isecWithPlane $ ngPlanes ng
      where
        isValid v = inRange v && inRayBounds v

        isecWithPlane p
#if DEBUG
          | isFinal <- all qtNearZero (isec - unQtVertex toV)
          , traceShow ( ("isecWith", isValid isec, isFinal, ngPosition ng)
                      , ("isec", isec)
                      , ("isecode", qtVertex2LocCode qt (QtVertex isec))
                      , ("lo", lo, "hi", hi)
                      , ("plane", p)
                      ) False = undefined
#endif
          | otherwise = isec
          where isec = lineHyperplaneIntersection
                         (unQtVertex lineDir) (unQtVertex fromV) p origin


        origin = liftA3 origin' (ngPosition ng) lo hi
          where
            origin' Up   _   hi'= hi'
            origin' Same lo' _  = lo'
            origin' Down lo' _  = lo'

        inRange v = all id (inRange' <$> ngPosition ng  <*> lo <*> hi <*> v)
          where
            inRange' Same lo' hi' = qtBetween  lo' hi'
            inRange' Down lo' _   = qtAlmostEq lo'
            inRange' Up   _   hi' = qtAlmostEq hi'

    inRayBounds v = all id (liftA3 qtBetweenC lo hi v)
      where
        lo = liftA2 min (unQtVertex fromV) (unQtVertex toV)
        hi = liftA2 max (unQtVertex fromV) (unQtVertex toV)


{-# INLINABLE traceRay #-}

checkNeighbor :: VectorSpace v => QtVertex v -> QtVertex v -> Neighbor v -> Bool
checkNeighbor (QtVertex from) (QtVertex to) ng
  = all id (liftA3 checkComp (ngPosition ng) from to) 
  where
    checkComp Same _ _       = True
    checkComp Down from' to' = from' > to'
    checkComp Up   from' to' = to'   > from'
{-# INLINE checkNeighbor #-}
    

traverseToCommonAncestor
  :: VectorSpace v
  => QuadTree v srid a -> TraversedNode v srid a -> LocCode v
  -> Maybe (TraversedNode v srid a)
traverseToCommonAncestor QuadTree{qtLevel=maxl} TNode{..} code
  | al > maxl = Nothing
  | otherwise = Just $ TNode { tNode     = findAncestor tNode tLevel
                             , tLevel    = al
                             , tCellCode = qtCellCode al tCellCode
                             }
  where
    al = commonAncestorLevel tCellCode code
    findAncestor !n !l
      | l==al     = n
      | otherwise = findAncestor (qParent n) (l+1)
{-# INLINE traverseToCommonAncestor #-}


commonAncestorLevel :: VectorSpace v => LocCode v -> LocCode v -> Level
commonAncestorLevel (LocCode a) (LocCode b)
  = Level (maximum (fmap componentLevel diff))
  where
    componentLevel d = finiteBitSize (undefined :: Word) - countLeadingZeros d
    diff             = liftA2 xor a b
{-# INLINE commonAncestorLevel #-}



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

neighbors :: forall v. HasHyperplanes v => Neighbors v
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


nearZeroBits :: Int
nearZeroBits = 4
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
