{-# LANGUAGE StandaloneDeriving
           , DataKinds
           , TypeOperators
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , TemplateHaskell
           , QuasiQuotes
           , MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , TypeSynonymInstances
           , RankNTypes
           , CPP
           , KindSignatures
           , DeriveFunctor
           , ScopedTypeVariables
           , FunctionalDependencies
           #-}
module Sigym4.Geometry.Types (
    Geometry (..)
  , LineString (..)
  , MultiLineString (..)
  , LinearRing (..)
  , Vertex
  , SqMatrix
  , Point (..)
  , MultiPoint (..)
  , Polygon (..)
  , MultiPolygon (..)
  , Triangle (..)
  , TIN (..)
  , PolyhedralSurface (..)
  , GeometryCollection (..)
  , Feature
  , FeatureCollection
  , FeatureT (..)
  , FeatureCollectionT (..)
  , VectorSpace (..)
  , HasOffset (..)
  , Pixel (..)
  , Size (..)
  , Offset (..)
  , RowMajor
  , ColumnMajor
  , Extent (..)
  , GeoTransform (..)
  , Raster (..)
  , rasterIndexPixel
  , unsafeRasterIndexPixel
  , rasterIndex
  , unsafeRasterIndex
  , northUpGeoTransform
  , GeoReference (..)
  , mkGeoReference
  , pointOffset
  , unsafePointOffset
  , grScalarSize
  , scalarSize
  , grForward
  , grBackward

  , mkLineString
  , mkLinearRing
  , mkPolygon
  , mkTriangle

  , HasCoordinates (..)
  , polygonRings
  , convertRasterOffsetType

  , gSrid
  , hasSrid
  , withSrid

  , eSize
  , invertible

  -- lenses & prisms
  , HasGeometry (..)
  , HasProperties (..)
  , HasFeatures (..)
  , HasPoints (..)
  , HasLineStrings (..)
  , HasOuterRing (..)
  , HasInnerRings (..)
  , HasPolygons (..)
  , HasTriangles (..)
  , HasGeometries (..)
  , HasVertex (..)

  , _GeoPoint
  , _GeoMultiPoint
  , _GeoLineString
  , _GeoMultiLineString
  , _GeoPolygon
  , _GeoMultiPolygon
  , _GeoTriangle
  , _GeoPolyhedralSurface
  , _GeoTIN
  , _GeoCollection

  , NoSrid

  -- re-exports
  , KnownNat
  , Nat
  , module V1
  , module V2
  , module V3
  , module V4
  , module VN
  , module Linear.Epsilon
) where

import Prelude hiding (product)
import Control.Lens hiding (coerce)
import Data.Coerce (coerce)
import Data.Distributive (Distributive)
import Data.Proxy (Proxy(..))
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData(rnf))
import qualified Data.Semigroup as SG
import Data.Foldable (product)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Foreign.Storable (Storable)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Language.Haskell.TH.Syntax
import Linear.V1 as V1
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.V4 as V4 hiding (vector, point)
import Linear.Epsilon
import Linear.V as VN hiding (dim)
import Linear.Matrix ((!*), (*!), inv22, inv33, inv44, det22, det33, det44)
import Linear.Metric (Metric)
import GHC.TypeLits

-- | A vertex
type Vertex v = v Double

-- | A square Matrix
type SqMatrix v = v (Vertex v)

-- | A vector space
class ( KnownNat (VsDim v), Num (Vertex v), Fractional (Vertex v)
      , Show (Vertex v), Eq (Vertex v), Ord (Vertex v), Epsilon (Vertex v)
      , U.Unbox (Vertex v), NFData (Vertex v)
      , Show (v Int), Eq (v Int)
      , Num (SqMatrix v), Show (SqMatrix v), Eq (SqMatrix v)
      , Metric v, Applicative v, Foldable v, Traversable v, Distributive v)
  => VectorSpace (v :: * -> *) where
    type VsDim v :: Nat

    inv :: SqMatrix v -> SqMatrix v
    det :: SqMatrix v -> Double

    dim :: Proxy v -> Int
    dim = const (reflectDim (Proxy :: Proxy (VsDim v)))
    {-# INLINE dim #-}

    coords :: v a -> [a]
    fromCoords :: [a] -> Maybe (v a)

    unsafeFromCoords :: [a] -> v a
    unsafeFromCoords = fromMaybe (error "unsafeFromCoords") . fromCoords
    {-# INLINE CONLIKE [1] unsafeFromCoords #-}

    liftTExp :: Lift a => v a -> Q (TExp (v a))

    {-# MINIMAL inv, det, coords , fromCoords, liftTExp #-}


{-# RULES

"unsafeFromCoords/V2" forall u v.
  unsafeFromCoords (u:v:[]) = V2 u v

"unsafeFromCoords/V3" forall u v z.
      unsafeFromCoords (u:v:z:[]) = V3 u v z

"unsafeFromCoords/V4"
 forall u v z w.
        unsafeFromCoords (u:v:z:w:[]) = V4 u v z w
  #-}


invertible :: VectorSpace v => SqMatrix v -> Bool
invertible = not . nearZero . det
{-# INLINE invertible #-}

instance VectorSpace V1 where
    type VsDim V1   = 1
    inv             = (pure 1 /)
    det (V1 (V1 u)) = u
    coords (V1 u)   = [u]
    fromCoords [u]  = Just (V1 u)
    fromCoords _    = Nothing
    liftTExp (V1 u) = [|| V1 u ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}

instance VectorSpace V2 where
    type VsDim V2 = 2
    inv = inv22
    det = det22
    coords (V2 u v)   = [u, v]
    fromCoords [u, v] = Just (V2 u v)
    fromCoords _      = Nothing
    liftTExp (V2 u v) = [|| V2 u v ||]

    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


instance VectorSpace V3 where
    type VsDim V3 = 3
    inv = inv33
    det = det33
    coords (V3 u v z)    = [u, v, z]
    fromCoords [u, v, z] = Just (V3 u v z)
    fromCoords _         = Nothing
    liftTExp (V3 u v z)  = [|| V3 u v z ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


instance VectorSpace V4 where
    type VsDim V4 = 4
    inv = inv44
    det = det44
    coords (V4 u v z w)     = [u, v, z, w]
    fromCoords [u, v, z, w] = Just (V4 u v z w)
    fromCoords _            = Nothing
    liftTExp (V4 u v z w)   = [|| V4 u v z w ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


instance KnownNat n => VectorSpace (V n) where
    type VsDim (V n) = n
    inv = error ("inv not implemented for V " ++ show (dim (Proxy :: Proxy (V n))))
    det = error ("det not implemented for V " ++ show (dim (Proxy :: Proxy (V n))))
    coords           = G.toList . toVector
    fromCoords       = fromVector . G.fromList
    liftTExp (V n)   = let l = G.toList n
                           d = dim (Proxy :: Proxy (V n))
                       in [|| V (G.fromListN d l) ||]
    {-# INLINE CONLIKE [1] coords #-}
    {-# INLINE fromCoords #-}
    {-# INLINE inv #-}
    {-# INLINE det #-}


newtype Offset (t :: OffsetType) = Offset {unOff :: Int}
  deriving (Eq, Show, Ord, Num)

data OffsetType = RowMajor | ColumnMajor

type RowMajor = 'RowMajor
type ColumnMajor = 'ColumnMajor

class VectorSpace v => HasOffset v (t :: OffsetType) where
    toOffset   :: Size v -> Pixel v -> Maybe (Offset t)
    fromOffset :: Size v -> Offset t -> Maybe (Pixel v)
    unsafeToOffset :: Size v -> Pixel v -> Offset t
    unsafeFromOffset :: Size v -> Offset t -> Pixel v

instance HasOffset V2 RowMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V2 sx sy = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p = Offset $ py * sx + px
      where V2 sx _  = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V2 sx sy)) o@(Offset o')
      | 0<=o' && o'<sx*sy = Just (unsafeFromOffset  s o)
      | otherwise        = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V2 sx _)) (Offset o)
      = Pixel (V2 (fromIntegral px) (fromIntegral py))
      where (py,px) =  o `divMod` sx
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V2 ColumnMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V2 sx sy = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p  = Offset $ px * sy + py
      where V2 _ sy  = unSize s
            V2 px py = fmap floor $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V2 sx sy)) o@(Offset o')
      | 0<=o' && o'<sx*sy = Just (unsafeFromOffset  s o)
      | otherwise        = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V2 _ sy)) (Offset o)
      = Pixel (V2 (fromIntegral px) (fromIntegral py))
      where (px,py) =  o `divMod` sy
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V3 RowMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy
      , 0<=pz && pz < sz = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V3 sx sy sz = unSize s
            V3 px py pz = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p  = Offset $ (pz * sx * sy) + (py * sx) + px
      where V3 sx sy _  = unSize s
            V3 px py pz = fmap floor $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V3 sx sy sz)) o@(Offset o')
      | 0<=o' && o'<sx*sy*sz = Just (unsafeFromOffset  s o)
      | otherwise            = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V3 sx sy _)) (Offset o)
      = Pixel (V3 (fromIntegral px) (fromIntegral py) (fromIntegral pz))
      where (pz, r) =  o `divMod` (sx*sy)
            (py,px) =  r `divMod` sx
    {-# INLINE unsafeFromOffset #-}

instance HasOffset V3 ColumnMajor where
    toOffset s p
      | 0<=px && px < sx
      , 0<=py && py < sy
      , 0<=pz && pz < sz = Just (unsafeToOffset s p)
      | otherwise        = Nothing
      where V3 sx sy sz = unSize s
            V3 px py pz = fmap floor $ unPx p
    {-# INLINE toOffset #-}
    unsafeToOffset s p = Offset $ (px * sz * sy) + (py * sz) + pz
      where V3 _  sy sz = unSize s
            V3 px py pz = fmap floor $ unPx p
    {-# INLINE unsafeToOffset #-}
    fromOffset s@(Size (V3 sx sy sz)) o@(Offset o')
      | 0<=o' && o'<sx*sy*sz = Just (unsafeFromOffset  s o)
      | otherwise            = Nothing
    {-# INLINE fromOffset #-}
    unsafeFromOffset (Size (V3 _ sy sz)) (Offset o)
      = Pixel (V3 (fromIntegral px) (fromIntegral py) (fromIntegral pz))
      where (px, r) =  o `divMod` (sz*sy)
            (py,pz) =  r `divMod` sz
    {-# INLINE unsafeFromOffset #-}

type NoSrid = 0


-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v (srid :: Nat) = Extent {eMin :: !(Vertex v), eMax :: !(Vertex v)}
deriving instance VectorSpace v => Eq (Extent v srid)
deriving instance VectorSpace v => Show (Extent v srid)

instance NFData (Vertex v) => NFData (Extent v srid) where
  rnf (Extent lo hi) = rnf lo `seq` rnf hi `seq` ()


eSize :: VectorSpace v => Extent v srid -> Vertex v
eSize e = eMax e - eMin e
{-# INLINE eSize #-}

instance VectorSpace v => SG.Semigroup (Extent v srid) where
    Extent a0 a1 <> Extent b0 b1
        = Extent (min <$> a0 <*> b0) (max <$> a1 <*> b1)
    {-# INLINE (<>) #-}

-- | A pixel is a newtype around a vertex
newtype Pixel v = Pixel {unPx :: Vertex v}
deriving instance VectorSpace v => Show (Pixel v)
deriving instance VectorSpace v => Eq (Pixel v)

newtype Size v = Size {unSize :: v Int}
deriving instance VectorSpace v => Eq (Size v)
deriving instance VectorSpace v => Show (Size v)

scalarSize :: VectorSpace v => Size v -> Int
scalarSize = product . unSize
{-# INLINE scalarSize #-}


-- A GeoTransform defines how we translate from geographic 'Vertex'es to
-- 'Pixel' coordinates and back. gtMatrix *must* be invertible so smart
-- constructors are provided
data GeoTransform v (srid :: Nat) = GeoTransform
      { gtMatrix :: !(SqMatrix v)
      , gtOrigin :: !(Vertex v)
      }
deriving instance VectorSpace v => Eq (GeoTransform v srid)
deriving instance VectorSpace v => Show (GeoTransform v srid)

northUpGeoTransform :: Extent V2 srid -> Size V2
                    -> Either String (GeoTransform V2 srid)
northUpGeoTransform e s
  | not isValidBox   = Left "northUpGeoTransform: invalid extent"
  | not isValidSize  = Left "northUpGeoTransform: invalid size"
  | otherwise        = Right $ GeoTransform matrix origin
  where
    isValidBox  = fmap (> 0) (eMax e - eMin e)  == pure True
    isValidSize = fmap (> 0) s'                 == pure True
    V2 x0 _     = eMin e
    V2 _ y1     = eMax e
    origin      = V2 x0 y1
    s'          = fmap fromIntegral $ unSize s
    V2 dx dy    = (eMax e - eMin e)/s'
    matrix      = V2 (V2 dx 0) (V2 0 (-dy))

gtForward :: VectorSpace v => GeoTransform v srid -> Point v srid -> Pixel v
gtForward gt (Point v) = Pixel $ m !* (v-v0)
  where v0  = gtOrigin gt
        m
#if ASSERTS
          | not (invertible (gtMatrix gt)) = error "gtMatrix is not invertible"
#endif
          | otherwise                      = inv (gtMatrix gt)


gtBackward :: VectorSpace v => GeoTransform v srid -> Pixel v -> Point v srid
gtBackward gt p = Point $ v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v srid = GeoReference
      { grTransform :: !(GeoTransform v srid)
      , grSize      :: !(Size v)
      }
deriving instance VectorSpace v => Eq (GeoReference v srid)
deriving instance VectorSpace v => Show (GeoReference v srid)


grScalarSize :: VectorSpace v => GeoReference v srid -> Int
grScalarSize = scalarSize . grSize
{-# INLINE grScalarSize #-}


pointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v srid -> Point v srid -> Maybe (Offset t)
pointOffset gr =  toOffset (grSize gr) . grForward gr
{-# INLINE pointOffset #-}

unsafePointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v srid -> Point v srid -> Offset t
unsafePointOffset gr =  unsafeToOffset (grSize gr) . grForward gr
{-# INLINE unsafePointOffset #-}


grForward :: VectorSpace v => GeoReference v srid -> Point v srid -> Pixel v
grForward gr = gtForward (grTransform gr)
{-# INLINE grForward #-}

grBackward :: VectorSpace v => GeoReference v srid -> Pixel v -> Point v srid
grBackward gr = gtBackward (grTransform gr)
{-# INLINE grBackward #-}


mkGeoReference :: Extent V2 srid -> Size V2 -> Either String (GeoReference V2 srid)
mkGeoReference e s = fmap (\gt -> GeoReference gt s) (northUpGeoTransform e s)

newtype Point v (srid :: Nat) = Point {_pointVertex:: Vertex v}
deriving instance VectorSpace v          => Show (Point v srid)
deriving instance VectorSpace v          => Eq (Point v srid)
deriving instance VectorSpace v          => Ord (Point v srid)
deriving instance NFData      (Vertex v) => NFData (Point v srid)
deriving instance Hashable    (Vertex v) => Hashable (Point v srid)
deriving instance Storable    (Vertex v) => Storable (Point v srid)

class HasVertex o a | o->a where
  vertex :: Lens' o a

instance HasVertex (Point v srid) (Vertex v) where
  vertex = lens coerce (const coerce)
  {-# INLINE vertex #-}


derivingUnbox "Point"
    [t| forall v srid. VectorSpace v => Point v srid -> Vertex v |]
    [| coerce |]
    [| coerce |]

derivingUnbox "Extent"
    [t| forall v srid. VectorSpace v => Extent v srid -> (Vertex v,Vertex v) |]
    [| \(Extent e0 e1) -> (e0,e1) |]
    [| \(e0,e1) -> Extent e0 e1 |]

derivingUnbox "Pixel"
    [t| forall v. VectorSpace v => Pixel v -> Vertex v |]
    [| coerce |]
    [| coerce |]

derivingUnbox "Offset"
    [t| forall t. Offset (t :: OffsetType) -> Int |]
    [| coerce |]
    [| coerce |]

newtype MultiPoint v srid = MultiPoint {
    _multiPointPoints :: U.Vector (Point v srid)
} deriving (Eq, Show)
makeFields ''MultiPoint

deriving instance VectorSpace v => NFData (MultiPoint v srid)

newtype LinearRing v srid
  = LinearRing {_linearRingPoints :: U.Vector (Point v srid)}
  deriving (Eq, Show)
makeFields ''LinearRing

deriving instance VectorSpace v => NFData (LinearRing v srid)

newtype LineString v srid
  = LineString {_lineStringPoints :: U.Vector (Point v srid)}
  deriving (Eq, Show)
makeFields ''LineString

deriving instance VectorSpace v => NFData (LineString v srid)

newtype MultiLineString v srid = MultiLineString {
    _multiLineStringLineStrings :: V.Vector (LineString v srid)
} deriving (Eq, Show)
makeFields ''MultiLineString

deriving instance VectorSpace v => NFData (MultiLineString v srid)

data Triangle v srid = Triangle !(Point v srid) !(Point v srid) !(Point v srid)
    deriving (Eq, Show)

instance VectorSpace v => NFData (Triangle v srid) where
  rnf (Triangle a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

derivingUnbox "Triangle"
    [t| forall v srid. VectorSpace v => Triangle v srid -> (Point v srid, Point v srid, Point v srid) |]
    [| \(Triangle a b c) -> (a, b, c) |]
    [| \(a, b, c) -> Triangle a b c|]

data Polygon v srid = Polygon {
    _polygonOuterRing  :: !(LinearRing v srid)
  , _polygonInnerRings :: !(V.Vector (LinearRing v srid))
} deriving (Eq, Show)
makeFields ''Polygon

instance VectorSpace v => NFData (Polygon v srid) where
  rnf (Polygon o r) = rnf o `seq` rnf r `seq` ()

newtype MultiPolygon v srid = MultiPolygon {
    _multiPolygonPolygons :: V.Vector (Polygon v srid)
} deriving (Eq, Show)
makeFields ''MultiPolygon

deriving instance VectorSpace v => NFData (MultiPolygon v srid)

newtype PolyhedralSurface v srid = PolyhedralSurface {
    _polyhedralSurfacePolygons :: V.Vector (Polygon v srid)
} deriving (Eq, Show)
makeFields ''PolyhedralSurface

deriving instance VectorSpace v => NFData (PolyhedralSurface v srid)

newtype TIN v srid = TIN {
    _tINTriangles :: U.Vector (Triangle v srid)
} deriving (Eq, Show)
makeFields ''TIN

deriving instance VectorSpace v => NFData (TIN v srid)

data Geometry v (srid::Nat)
    = GeoPoint !(Point v srid)
    | GeoMultiPoint !(MultiPoint v srid)
    | GeoLineString !(LineString v srid)
    | GeoMultiLineString !(MultiLineString v srid)
    | GeoPolygon !(Polygon v srid)
    | GeoMultiPolygon !(MultiPolygon v srid)
    | GeoTriangle !(Triangle v srid)
    | GeoPolyhedralSurface !(PolyhedralSurface v srid)
    | GeoTIN !(TIN v srid)
    | GeoCollection !(GeometryCollection v srid)
    deriving (Eq, Show)

instance VectorSpace v => NFData (Geometry v srid) where
  rnf (GeoPoint g)             = rnf g
  rnf (GeoMultiPoint g)        = rnf g
  rnf (GeoLineString g)        = rnf g
  rnf (GeoMultiLineString g)   = rnf g
  rnf (GeoPolygon g)           = rnf g
  rnf (GeoMultiPolygon g)      = rnf g
  rnf (GeoTriangle g)          = rnf g
  rnf (GeoPolyhedralSurface g) = rnf g
  rnf (GeoTIN g)               = rnf g
  rnf (GeoCollection g)        = rnf g

newtype GeometryCollection v srid = GeometryCollection {
    _geometryCollectionGeometries :: V.Vector (Geometry v srid)
} deriving (Eq, Show)
makeFields ''GeometryCollection
makePrisms ''Geometry

deriving instance VectorSpace v => NFData (GeometryCollection v srid)

gSrid :: KnownNat srid => proxy srid -> Integer
gSrid = natVal
{-# INLINE gSrid #-}

withSrid
  :: Integer
  -> (forall srid. KnownNat srid => Proxy srid -> a)
  -> Maybe a
withSrid srid f
  = case someNatVal srid of
      Just (SomeNat a) -> Just (f a)
      Nothing          -> Nothing

hasSrid :: KnownNat srid => Geometry v srid -> Bool
hasSrid = (/= 0) . gSrid

mkLineString :: VectorSpace v => [Point v srid] -> Maybe (LineString v srid)
mkLineString ls
  | U.length v >= 2 = Just $ LineString v
  | otherwise = Nothing
  where v = U.fromList ls

mkLinearRing :: VectorSpace v => [Point v srid] -> Maybe (LinearRing v srid)
mkLinearRing ls
  | U.length v >= 4, U.last v == U.head v = Just $ LinearRing v
  | otherwise = Nothing
  where v = U.fromList ls

mkPolygon :: [LinearRing v srid] -> Maybe (Polygon v srid)
mkPolygon (oRing:rings_) = Just $ Polygon oRing $ V.fromList rings_
mkPolygon _ = Nothing

mkTriangle :: VectorSpace v
  => Point v srid -> Point v srid -> Point v srid -> Maybe (Triangle v srid)
mkTriangle a b c | a/=b, b/=c, a/=c = Just $ Triangle a b c
                 | otherwise = Nothing

class HasCoordinates o a | o -> a where
  coordinates :: o -> a

instance VectorSpace v => HasCoordinates (Point v srid) [Double] where
  coordinates = views vertex coords
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (MultiPoint v srid) [[Double]] where
  coordinates = views points (G.toList . V.map coordinates . G.convert)
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (LineString v srid) [[Double]] where
  coordinates = views points coordinates
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (MultiLineString v srid) [[[Double]]] where
  coordinates = views lineStrings (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (LinearRing v srid) [[Double]] where
  coordinates = views points coordinates
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (Polygon v srid) [[[Double]]] where
  coordinates = V.toList . V.map coordinates . polygonRings
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (MultiPolygon v srid) [[[[Double]]]] where
  coordinates = views polygons (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (PolyhedralSurface v srid) [[[[Double]]]] where
  coordinates = views polygons (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

polygonRings :: Polygon v srid -> V.Vector (LinearRing v srid)
polygonRings (Polygon ir rs) = V.cons ir rs
{-# INLINE polygonRings #-}

instance VectorSpace v => HasCoordinates (Triangle v srid) [[Double]] where
  coordinates (Triangle a b c) = map coordinates [a, b, c, a]
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (TIN v srid) [[[Double]]] where
  coordinates = views triangles (V.toList . V.map coordinates . G.convert)
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (U.Vector (Point v srid)) [[Double]] where
  coordinates = V.toList . V.map coordinates . V.convert
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (V.Vector (Point v srid)) [[Double]] where
  coordinates = V.toList . V.map coordinates
  {-# INLINE coordinates #-}

-- | A feature of 'GeometryType' t, vertex type 'v' and associated data 'd'
data FeatureT (g :: (* -> *) -> Nat -> *) v (srid::Nat) d = Feature {
    _featureTGeometry :: !(g v srid)
  , _featureTProperties :: !d
  } deriving (Eq, Show, Functor)
makeFields ''FeatureT

instance (NFData d, NFData (g v srid)) => NFData (FeatureT g v srid d) where
  rnf (Feature g v) = rnf g `seq` rnf v `seq` ()

type Feature = FeatureT Geometry

derivingUnbox "UnboxFeature"
    [t| forall g v srid a. (VectorSpace v, U.Unbox a, U.Unbox (g v srid))
        => FeatureT g v srid a -> (g v srid, a) |]
    [| \(Feature p v) -> (p,v) |]
    [| \(p,v) -> Feature p v|]

newtype FeatureCollectionT (g :: (* -> *) -> Nat -> *) v (srid::Nat) d
  = FeatureCollection {
    _featureCollectionTFeatures :: [FeatureT g v srid d]
  } deriving (Eq, Show, Functor)
makeFields ''FeatureCollectionT

type FeatureCollection = FeatureCollectionT Geometry

instance Monoid (FeatureCollectionT g v srid d) where
    mempty = FeatureCollection mempty
    (FeatureCollection as) `mappend` (FeatureCollection bs)
        = FeatureCollection $ as `mappend` bs

data Raster vs (t :: OffsetType) srid v a
  = Raster {
      rGeoReference :: !(GeoReference vs srid)
    , rData         :: !(v a)
    } deriving (Eq, Show)

rasterIndex
  :: forall vs t srid v a. (HasOffset vs t)
  => Raster vs t srid v a -> Point vs srid -> Maybe Int
rasterIndex r p = fmap unOff offset
  where
    offset = pointOffset (rGeoReference r) p :: Maybe (Offset t)
{-# INLINE rasterIndex #-}

unsafeRasterIndex
  :: forall vs t srid v a. (HasOffset vs t)
  => Raster vs t srid v a -> Point vs srid -> Int
unsafeRasterIndex r p = unOff offset
  where
    offset = unsafePointOffset (rGeoReference r) p :: Offset t
{-# INLINE unsafeRasterIndex #-}

rasterIndexPixel
  :: forall vs t srid v a. (HasOffset vs t)
  => Raster vs t srid v a -> Pixel vs -> Maybe Int
rasterIndexPixel r px = fmap unOff offset
  where
    offset = toOffset (grSize (rGeoReference r)) px :: Maybe  (Offset t)
{-# INLINE rasterIndexPixel #-}

unsafeRasterIndexPixel
  :: forall vs t srid v a. (HasOffset vs t)
  => Raster vs t srid v a -> Pixel vs -> Int
unsafeRasterIndexPixel r px = unOff offset
  where
    offset = unsafeToOffset (grSize (rGeoReference r)) px :: Offset t
{-# INLINE unsafeRasterIndexPixel #-}


convertRasterOffsetType
  :: forall vs t1 t2 srid v a. (G.Vector v a, HasOffset vs t1, HasOffset vs t2)
  => Raster vs t1 srid v a -> Raster vs t2 srid v a
convertRasterOffsetType r = r {rData = G.generate n go}
  where go i = let px        = unsafeFromOffset s (Offset i :: Offset t2)
                   Offset i' = unsafeToOffset s px :: Offset t1
               in rd `G.unsafeIndex` i'
        s = grSize (rGeoReference r)
        rd = rData r
        n  = G.length rd
