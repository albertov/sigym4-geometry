{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sigym4.Geometry.QuadTree.Internal.Types (
    QuadTree      (..)
  , QtError       (..)
  , Node          (..)
  , QNode         (..)
  , Level         (..)
  , LocCode       (..)
  , Quadrant      (..)
  , Half          (..)
  , Neighbor      (..)
  , NeighborDir   (..)
  , QtVertex      (..)
  , TraversedNode (..)

  , Neighbors
  , NeighborPosition
  , Box

  , generate
  , generate2
  , generate3
  , empty
  , qtMinBox

  , calculateMinBox
  , getChild
  , getChildAtLevel
  , quadrantAtLevel
  , setChildBits
  , oppositePosition
  , maxValue
  , neighborsDefault

  , mkNeighbors
  , machineEpsilonAndLevel
  , genQNode
  , genNode
  , rootParent
  , innerExtent
  , outerExtent
) where

import Control.Monad (liftM, replicateM, guard)
import Control.Monad.Fix (MonadFix(mfix))
import Control.Applicative (liftA2, liftA3)
import Control.DeepSeq (NFData(..))
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

data QtError
  = QtInvalidLevel
  | QtCannotGrow
  deriving (Show, Eq, Enum)

data QuadTree (v :: * -> *) crs a
  = QuadTree {
      qtRoot   :: QNode v crs a
    , qtExtent :: {-# UNPACK #-} !(Extent v crs)
    , qtLevel  :: {-# UNPACK #-} !Level
  }

empty :: VectorSpace v => a -> Point v crs -> Box v -> QuadTree v crs a
empty a (Point c) minBox = QuadTree (QLeaf rootParent a) ext (Level 0)
  where ext = Extent (c - minBox/2) (c + minBox/2)

instance VectorSpace v => Traversable (QuadTree v crs) where
  {-# INLINE traverse #-}
  traverse f qt = QuadTree <$> go rootParent (qtRoot qt)
                           <*> pure (qtExtent qt)
                           <*> pure (qtLevel qt)
    where
      go p !QLeaf{qData=a} = QLeaf <$> pure p <*> f a
      go p !QNode{qChildren=cs}
        = let n = QNode {qParent=p, qChildren=mkEmptyArr p}
              setChildren elems = runST $ do
                -- we unsafely thaw children array and update it so we can
                -- traverse in one pass. Else we would need to make another
                -- traversal to to update qParents after children are created
                cs' <- unsafeThawArray (qChildren n)
                let loop !_ []     = return n
                    loop !i (x:xs) = writeArray cs' i x >> loop (i+1) xs
                loop 0 elems
          in fmap setChildren (traverse (go n . indexArray cs) (childIxes px))
      -- initialize array with 't' to make sure it's not floated out and shared
      mkEmptyArr t = runST (newArray (numChildren px) t >>= unsafeFreezeArray)
      px = Proxy :: Proxy v

instance VectorSpace v => Foldable (QuadTree v crs) where
  foldr  f z = foldr  f z . qtRoot
  foldl  f z = foldl  f z . qtRoot
  foldl' f z = foldl' f z . qtRoot

instance (VectorSpace v, NFData a) => NFData (QuadTree v crs a) where
  rnf (QuadTree r e l) = rnf r `seq` rnf e `seq` rnf l `seq` ()

instance (VectorSpace v, Eq a) => Eq (QuadTree v crs a) where
  (==) a b = qtExtent a == qtExtent b
          && qtLevel  a == qtLevel b
          && toList   a == toList b

instance VectorSpace v => Functor (QuadTree v crs) where
  {-# INLINE fmap#-}
  fmap f qt@QuadTree{qtRoot=root} = qt {qtRoot=go rootParent root}
    where
      go p QLeaf{qData=a} = QLeaf {qData=f a, qParent=p}
      go p QNode{qChildren=children}
        = let n = QNode { qChildren = runIdentity (generateChildren genChild)
                        , qParent   = p}
              genChild = return . go n . indexArray children
          in n

childIxes :: VectorSpace v => Proxy v -> [Int]
childIxes p = enumFromTo 0 (numChildren p - 1)
{-# INLINE childIxes#-}

generateChildren
  :: forall v crs m a. (VectorSpace v, Monad m)
  => (Int -> m (QNode v crs a)) -> m (Array (QNode v crs a))
generateChildren f = do
  elems <- mapM f (childIxes px)
  return $! runST $ do
    cs <- newArray (numChildren px) undefined
    let loop !_ []     = return ()
        loop !i (x:xs) = writeArray cs i x >> loop (i+1) xs
    loop 0 elems
    unsafeFreezeArray cs
  where px = Proxy :: Proxy v
{-# INLINE generateChildren #-}

numChildren :: VectorSpace v => Proxy v -> Int
numChildren p = 2 ^ dim p
{-# INLINE numChildren #-}

getChild
  :: VectorSpace v => Array (QNode v crs a) -> Quadrant v -> QNode v crs a
getChild c = runIdentity . indexArrayM c . fromEnum
{-# INLINE getChild #-}


getChildAtLevel
  :: VectorSpace v
  => Array (QNode v crs a) -> Level -> LocCode v -> QNode v crs a
getChildAtLevel cs lv@(Level l) (LocCode c) = indexArray cs ix
  where
    !ix          = snd (foldl' go (negate l,0) c)
    !m           =  maxValue lv
    go (!i,!s) v = (i+1, s + ((v .&. m) `rotate` i))
{-# INLINE getChildAtLevel #-}

rootParent :: QNode v crs a
rootParent = error "QuadTree: should not happen, tried to get root's parent"


instance VectorSpace v => Show (QuadTree v crs a) where
  show QuadTree{..} = concat ([
     "QuadTree { qtExtent = ", show qtExtent, ","
    ,          " qtLevel = ", show qtLevel, " }"] :: [String])

type Box v = Vertex v

data QNode (v :: * -> *) crs a
  = QLeaf { qParent   :: QNode v crs a   -- undefined if root
          , qData     :: a
          }
  | QNode { qParent   :: QNode v crs a   -- undefined if root
          , qChildren :: {-# UNPACK #-} !(Array (QNode v crs a))
          }

instance (VectorSpace v, NFData a) => NFData (QNode v crs a) where
  rnf = rnf . toList

instance (Show a, VectorSpace v) => Show (QNode v crs a) where
  show QLeaf{..} = concat (["QLeaf {qData = ", show qData, "}"] :: [String])
  show n@QNode{} = concat ([ "QNode {qChildren = "
                             , show (toList n), " }"] :: [String])

instance VectorSpace v => Foldable (QNode v crs) where
  {-# INLINE foldr #-}
  foldr f z QLeaf{qData=a}      = f a z
  foldr f z QNode{qChildren=cs} = loop (numChildren (Proxy :: Proxy v)-1) z
    where loop !i acc
            | i >= 0    = loop (i-1) (foldr f acc (indexArray cs i))
            | otherwise = acc

  {-# INLINE foldl #-}
  foldl f z QLeaf{qData=a}      = f z a
  foldl f z QNode{qChildren=cs} = loop 0 z
    where loop !i acc
            | i < n     = loop (i+1) (foldl f acc (indexArray cs i))
            | otherwise = acc
          !n = numChildren (Proxy :: Proxy v)

  {-# INLINE foldl' #-}
  foldl' f !z QLeaf{qData=a}      = f z a
  foldl' f !z QNode{qChildren=cs} = loop 0 z
    where loop !i !acc
            | i < n     = loop (i+1) (foldl' f acc (indexArray cs i))
            | otherwise = acc
          !n = numChildren (Proxy :: Proxy v)

data Node m v crs a
  = Leaf a
  | Node (Extent v crs -> m (a, Node m v crs a))

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


quadrantAtLevel :: VectorSpace v => Level -> LocCode v -> Quadrant v
quadrantAtLevel (Level l) = Quadrant . fmap toHalf . unLocCode
  where toHalf v = if v `testBit` l then Second else First
{-# INLINE quadrantAtLevel #-}

setChildBits:: VectorSpace v => Level -> Quadrant v -> LocCode v -> LocCode v
setChildBits l (Quadrant q) (LocCode code)
  = LocCode (liftA2 (.|.) code val)
  where
    val = fmap (\c -> case c of {Second->maxValue l; First->0}) q
{-# INLINE setChildBits #-}


data NeighborDir = Down | Same | Up
  deriving (Show, Eq, Enum, Bounded)

instance Lift (NeighborDir) where
    lift Up   = [| Up |]
    lift Down = [| Down |]
    lift Same = [| Same |]

data Neighbor v = Ng { ngPosition :: NeighborPosition v
                     , ngPlanes   :: HyperPlaneDirections v
                     }

deriving instance (Show (HyperPlaneDirections v), Show (NeighborPosition v))
  => Show (Neighbor v)


type Neighbors v = [Neighbor v]
type NeighborPosition v = v NeighborDir

oppositePosition :: VectorSpace v => NeighborPosition v -> NeighborPosition v
oppositePosition = fmap (\p -> case p of {Up->Down; Down->Up; _->p})
{-# INLINE oppositePosition #-}


newtype Level = Level {unLevel :: Int}
  deriving (Eq, Ord, Show, NFData)


instance Bounded Level where
  maxBound = Level ((finiteBitSize (undefined::Int) `unsafeShiftR` 1) - 1)
  minBound = Level 0
  {-# INLINE maxBound #-}
  {-# INLINE minBound #-}


newtype LocCode v = LocCode {unLocCode :: v Int}

deriving instance Num (v Int) => Num (LocCode v)
deriving instance Eq (v Int) => Eq (LocCode v)
deriving instance Show (v Int) => Show (LocCode v)

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

    mkDirections :: NeighborPosition v -> HyperPlaneDirections v
    mkDirections pos = unsafeFromCoords (take numDirs (must ++ perhaps))
       where
        (_, must, perhaps)          = foldl' makeDirection (0, [], []) pos
        makeDirection (!i,m,p) Same = (i+1, unit i:m, p       )
        makeDirection (!i,m,p) _    = (i+1, m       , unit i:p)
        numDirs                     = dim (Proxy :: Proxy v) - 1
        unit                        = (!!) (coords (identity :: SqMatrix v))


mkNeighbors
  :: forall v. HasHyperplanes v => Q (TExp (Neighbors v))
mkNeighbors = [|| $$(liftM (TExp . ListE) ns) ||]
  where ns = mapM (fmap unType . liftNeighbor) (neighborsDefault :: Neighbors v)

liftNeighbor
  :: forall v. HasHyperplanes v => Neighbor v -> Q (TExp (Neighbor v))
liftNeighbor (Ng v ns) = [|| Ng $$(liftTExp v) (unsafeFromCoords $$(planes)) ||]
  where
    planes = liftM (TExp . ListE) (mapM (fmap unType . liftTExp) (coords ns))

data TraversedNode (v :: * -> *) crs a
  = TNode
    { tLevel    :: {-# UNPACK #-} !Level
    , tNode     :: !(QNode v crs a)
    , tCellCode :: LocCode v
    }

instance Eq (LocCode v) => Eq (TraversedNode v crs a) where
  a == b = tCellCode a == tCellCode b && tLevel a == tLevel b

instance Show (LocCode v) => Show (TraversedNode v crs a) where
  show TNode{..} = concat (["TNode { tLevel = ", show tLevel
                           ,      ", tCellCode = ", show tCellCode, " }"])


innerExtent :: VectorSpace v => Quadrant v -> Extent v crs -> Extent v crs
innerExtent (Quadrant qv) (Extent lo hi) = Extent lo' hi'
  where
    lo'             = liftA3 mkLo qv lo hi
    hi'             = liftA3 mkHi qv lo hi
    mkLo First  l _ = l
    mkLo Second l h = (l+h) / 2
    mkHi First  l h = (l+h) / 2
    mkHi Second _ h = h
{-# INLINE innerExtent #-}


outerExtent :: VectorSpace v => Quadrant v -> Extent v crs -> Extent v crs
outerExtent (Quadrant qv) (Extent lo hi) = Extent lo' hi'
  where
    lo'             = liftA3 mkLo qv lo hi
    hi'             = liftA3 mkHi qv lo hi
    mkLo First  l _ = l
    mkLo Second l h = 2*l - h
    mkHi First  l h = 2*h - l
    mkHi Second _ h = h
{-# INLINE outerExtent #-}


generate2
  :: (MonadFix m, VectorSpace v)
  => Node m v crs a -> Extent v crs -> Level
  -> m (Either QtError (QuadTree v crs a))
generate2 build ext level
  | level > maxBound || level < minBound = return (Left QtInvalidLevel)
  | otherwise
  = Right <$> (QuadTree <$> genNode rootParent ext level build
                        <*> pure ext
                        <*> pure level)

generate
  :: (MonadFix m, VectorSpace v)
  => Node m v crs a -> Extent v crs -> Box v
  -> m (Either QtError (QuadTree v crs a))
generate build ext minBox = generate2 build effectiveExt level
  where (effectiveExt, level) = effectiveExtAndLevel ext minBox

generate3
  :: (MonadFix m, VectorSpace v)
  => (Extent v crs -> Node m v crs a)
  -> Extent v crs -> Box v
  -> m (Either QtError (QuadTree v crs a))
generate3 build ext = generate (build ext) ext

effectiveExtAndLevel
  :: VectorSpace v => Extent v crs -> Box v -> (Extent v crs, Level)
effectiveExtAndLevel ext minBox = (effectiveExt, level)
  where effectiveExt = Extent (eMin ext) (eMin ext + delta)
        delta  = fmap (* maxVal) minBox
        maxVal = fromIntegral (maxValue level)
        level  = Level (ceiling (logBase 2 nCells))
        nCells = maximum (eSize ext / minBox)

genNode
  :: (MonadFix m, VectorSpace v)
  => QNode v crs a -> Extent v crs -> Level -> Node m v crs a
  -> m (QNode v crs a)
genNode parent _   _ (Leaf v) = return (QLeaf parent v)
genNode parent ext level (Node f)
  | level > minBound = mfix (\node -> genQNode parent $ \q -> do
                               next <- liftM snd (f (innerExtent q ext))
                               let level' = Level (unLevel level - 1)
                               genNode node (innerExtent q ext) level' next)
  | otherwise        = liftM (QLeaf parent . fst) (f ext)

genQNode
  :: forall m v crs a. (MonadFix m, VectorSpace v)
  => QNode v crs a -> (Quadrant v -> m (QNode v crs a))
  -> m (QNode v crs a)
genQNode parent f = liftM (QNode parent) (generateChildren (f . toEnum))


maxValue :: Level -> Int
maxValue (Level l) = 1 `unsafeShiftL` l
{-# INLINE maxValue #-}

qtMinBox :: VectorSpace v => QuadTree v crs a -> Box v
qtMinBox QuadTree{qtLevel=l, qtExtent=e} = calculateMinBox e l

calculateMinBox :: VectorSpace v => Extent v crs -> Level -> Box v
calculateMinBox e l
  = fmap (/ (fromIntegral (maxValue l))) (eSize e)
{-# INLINE calculateMinBox #-}


calculatedEpsilonAndLevel :: (Ord a, Fractional a) => (Int,a)
calculatedEpsilonAndLevel = go 0 1
  where go !n !e | e+1>1     = go (n+1) (e*0.5)
                 | otherwise = (n,e)


machineEpsilonAndLevel
  :: (Lift a, Ord a, Fractional a)
  => Int -> Q (TExp (Int,a))
machineEpsilonAndLevel f
  = let (l,e) = calculatedEpsilonAndLevel
        r     = (l-f, e * 2^f)
    in [|| r ||]
