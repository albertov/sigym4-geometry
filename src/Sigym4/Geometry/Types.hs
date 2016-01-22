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
           , ExistentialQuantification
           , UndecidableInstances
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

  , HasSrid (..)
  , crs
  , hasCrs
  , hasSrid
  , withCrs

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

  , NoCrs
  , SomeFeature
  , SomeFeatureCollection
  , SomeFeatureT (..)
  , SomeFeatureCollectionT (..)
  , SomeGeometry (..)
  , HasSameCrs (..)

  -- re-exports
  , KnownSymbol
  , Symbol
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
import Data.Maybe (fromMaybe, isJust)
import Data.List (stripPrefix)
import Text.Read (readMaybe)
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
import Unsafe.Coerce (unsafeCoerce)

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

type NoCrs = ""


-- | An extent in v space is a pair of minimum and maximum vertices
data Extent v (crs :: Symbol) = Extent {eMin :: !(Vertex v), eMax :: !(Vertex v)}
deriving instance VectorSpace v => Eq (Extent v crs)
deriving instance VectorSpace v => Show (Extent v crs)

instance NFData (Vertex v) => NFData (Extent v crs) where
  rnf (Extent lo hi) = rnf lo `seq` rnf hi `seq` ()


eSize :: VectorSpace v => Extent v crs -> Vertex v
eSize e = eMax e - eMin e
{-# INLINE eSize #-}

instance VectorSpace v => SG.Semigroup (Extent v crs) where
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
data GeoTransform v (crs :: Symbol) = GeoTransform
      { gtMatrix :: !(SqMatrix v)
      , gtOrigin :: !(Vertex v)
      }
deriving instance VectorSpace v => Eq (GeoTransform v crs)
deriving instance VectorSpace v => Show (GeoTransform v crs)

northUpGeoTransform :: Extent V2 crs -> Size V2
                    -> Either String (GeoTransform V2 crs)
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

gtForward :: VectorSpace v => GeoTransform v crs -> Point v crs -> Pixel v
gtForward gt (Point v) = Pixel $ m !* (v-v0)
  where v0  = gtOrigin gt
        m
#if ASSERTS
          | not (invertible (gtMatrix gt)) = error "gtMatrix is not invertible"
#endif
          | otherwise                      = inv (gtMatrix gt)


gtBackward :: VectorSpace v => GeoTransform v crs -> Pixel v -> Point v crs
gtBackward gt p = Point $ v0 + (unPx p) *! m
  where m  = gtMatrix gt
        v0 = gtOrigin gt

data GeoReference v crs = GeoReference
      { grTransform :: !(GeoTransform v crs)
      , grSize      :: !(Size v)
      }
deriving instance VectorSpace v => Eq (GeoReference v crs)
deriving instance VectorSpace v => Show (GeoReference v crs)


grScalarSize :: VectorSpace v => GeoReference v crs -> Int
grScalarSize = scalarSize . grSize
{-# INLINE grScalarSize #-}


pointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v crs -> Point v crs -> Maybe (Offset t)
pointOffset gr =  toOffset (grSize gr) . grForward gr
{-# INLINE pointOffset #-}

unsafePointOffset :: (HasOffset v t, VectorSpace v)
  => GeoReference v crs -> Point v crs -> Offset t
unsafePointOffset gr =  unsafeToOffset (grSize gr) . grForward gr
{-# INLINE unsafePointOffset #-}


grForward :: VectorSpace v => GeoReference v crs -> Point v crs -> Pixel v
grForward gr = gtForward (grTransform gr)
{-# INLINE grForward #-}

grBackward :: VectorSpace v => GeoReference v crs -> Pixel v -> Point v crs
grBackward gr = gtBackward (grTransform gr)
{-# INLINE grBackward #-}


mkGeoReference :: Extent V2 crs -> Size V2 -> Either String (GeoReference V2 crs)
mkGeoReference e s = fmap (\gt -> GeoReference gt s) (northUpGeoTransform e s)

newtype Point v (crs :: Symbol) = Point {_pointVertex:: Vertex v}
deriving instance VectorSpace v          => Show (Point v crs)
deriving instance VectorSpace v          => Eq (Point v crs)
deriving instance VectorSpace v          => Ord (Point v crs)
deriving instance NFData      (Vertex v) => NFData (Point v crs)
deriving instance Hashable    (Vertex v) => Hashable (Point v crs)
deriving instance Storable    (Vertex v) => Storable (Point v crs)

class HasVertex o a | o->a where
  vertex :: Lens' o a

instance HasVertex (Point v crs) (Vertex v) where
  vertex = lens coerce (const coerce)
  {-# INLINE vertex #-}


derivingUnbox "Point"
    [t| forall v crs. VectorSpace v => Point v crs -> Vertex v |]
    [| coerce |]
    [| coerce |]

derivingUnbox "Extent"
    [t| forall v crs. VectorSpace v => Extent v crs -> (Vertex v,Vertex v) |]
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

newtype MultiPoint v crs = MultiPoint {
    _multiPointPoints :: U.Vector (Point v crs)
} deriving (Eq, Show)
makeFields ''MultiPoint

deriving instance VectorSpace v => NFData (MultiPoint v crs)

newtype LinearRing v crs
  = LinearRing {_linearRingPoints :: U.Vector (Point v crs)}
  deriving (Eq, Show)
makeFields ''LinearRing

deriving instance VectorSpace v => NFData (LinearRing v crs)

newtype LineString v crs
  = LineString {_lineStringPoints :: U.Vector (Point v crs)}
  deriving (Eq, Show)
makeFields ''LineString

deriving instance VectorSpace v => NFData (LineString v crs)

newtype MultiLineString v crs = MultiLineString {
    _multiLineStringLineStrings :: V.Vector (LineString v crs)
} deriving (Eq, Show)
makeFields ''MultiLineString

deriving instance VectorSpace v => NFData (MultiLineString v crs)

data Triangle v crs = Triangle !(Point v crs) !(Point v crs) !(Point v crs)
    deriving (Eq, Show)

instance VectorSpace v => NFData (Triangle v crs) where
  rnf (Triangle a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

derivingUnbox "Triangle"
    [t| forall v crs. VectorSpace v => Triangle v crs -> (Point v crs, Point v crs, Point v crs) |]
    [| \(Triangle a b c) -> (a, b, c) |]
    [| \(a, b, c) -> Triangle a b c|]

data Polygon v crs = Polygon {
    _polygonOuterRing  :: !(LinearRing v crs)
  , _polygonInnerRings :: !(V.Vector (LinearRing v crs))
} deriving (Eq, Show)
makeFields ''Polygon

instance VectorSpace v => NFData (Polygon v crs) where
  rnf (Polygon o r) = rnf o `seq` rnf r `seq` ()

newtype MultiPolygon v crs = MultiPolygon {
    _multiPolygonPolygons :: V.Vector (Polygon v crs)
} deriving (Eq, Show)
makeFields ''MultiPolygon

deriving instance VectorSpace v => NFData (MultiPolygon v crs)

newtype PolyhedralSurface v crs = PolyhedralSurface {
    _polyhedralSurfacePolygons :: V.Vector (Polygon v crs)
} deriving (Eq, Show)
makeFields ''PolyhedralSurface

deriving instance VectorSpace v => NFData (PolyhedralSurface v crs)

newtype TIN v crs = TIN {
    _tINTriangles :: U.Vector (Triangle v crs)
} deriving (Eq, Show)
makeFields ''TIN

deriving instance VectorSpace v => NFData (TIN v crs)

data Geometry v (crs::Symbol)
    = GeoPoint !(Point v crs)
    | GeoMultiPoint !(MultiPoint v crs)
    | GeoLineString !(LineString v crs)
    | GeoMultiLineString !(MultiLineString v crs)
    | GeoPolygon !(Polygon v crs)
    | GeoMultiPolygon !(MultiPolygon v crs)
    | GeoTriangle !(Triangle v crs)
    | GeoPolyhedralSurface !(PolyhedralSurface v crs)
    | GeoTIN !(TIN v crs)
    | GeoCollection !(GeometryCollection v crs)
    deriving (Eq, Show)

instance VectorSpace v => NFData (Geometry v crs) where
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

newtype GeometryCollection v crs = GeometryCollection {
    _geometryCollectionGeometries :: V.Vector (Geometry v crs)
} deriving (Eq, Show)
makeFields ''GeometryCollection
makePrisms ''Geometry

deriving instance VectorSpace v => NFData (GeometryCollection v crs)

class KnownSymbol crs => HasSrid (crs :: Symbol) where
  srid :: proxy crs -> Maybe Int
  srid p = stripPrefix prefix (crs p) >>= readMaybe
    where
      prefix = "urn:ogc:def:crs:EPSG::"

instance HasSrid "" where
  srid _ = Nothing

hasSrid :: HasSrid crs => proxy crs -> Bool
hasSrid = isJust . srid

crs :: KnownSymbol crs => proxy crs -> String
crs = symbolVal
{-# INLINE crs #-}

withCrs
  :: String
  -> (forall crs. KnownSymbol crs => Proxy crs -> a)
  -> a
withCrs c f
  = case someSymbolVal c of
      SomeSymbol a -> f a

hasCrs :: KnownSymbol crs => Geometry v crs -> Bool
hasCrs = (/= "") . crs

mkLineString :: VectorSpace v => [Point v crs] -> Maybe (LineString v crs)
mkLineString ls
  | U.length v >= 2 = Just $ LineString v
  | otherwise = Nothing
  where v = U.fromList ls

mkLinearRing :: VectorSpace v => [Point v crs] -> Maybe (LinearRing v crs)
mkLinearRing ls
  | U.length v >= 4, U.last v == U.head v = Just $ LinearRing v
  | otherwise = Nothing
  where v = U.fromList ls

mkPolygon :: [LinearRing v crs] -> Maybe (Polygon v crs)
mkPolygon (oRing:rings_) = Just $ Polygon oRing $ V.fromList rings_
mkPolygon _ = Nothing

mkTriangle :: VectorSpace v
  => Point v crs -> Point v crs -> Point v crs -> Maybe (Triangle v crs)
mkTriangle a b c | a/=b, b/=c, a/=c = Just $ Triangle a b c
                 | otherwise = Nothing

class HasCoordinates o a | o -> a where
  coordinates :: o -> a

instance VectorSpace v => HasCoordinates (Point v crs) [Double] where
  coordinates = views vertex coords
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (MultiPoint v crs) [[Double]] where
  coordinates = views points (G.toList . V.map coordinates . G.convert)
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (LineString v crs) [[Double]] where
  coordinates = views points coordinates
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (MultiLineString v crs) [[[Double]]] where
  coordinates = views lineStrings (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (LinearRing v crs) [[Double]] where
  coordinates = views points coordinates
  {-# INLINE coordinates #-}

instance VectorSpace v => HasCoordinates (Polygon v crs) [[[Double]]] where
  coordinates = V.toList . V.map coordinates . polygonRings
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (MultiPolygon v crs) [[[[Double]]]] where
  coordinates = views polygons (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (PolyhedralSurface v crs) [[[[Double]]]] where
  coordinates = views polygons (G.toList . G.map coordinates)
  {-# INLINE coordinates #-}

polygonRings :: Polygon v crs -> V.Vector (LinearRing v crs)
polygonRings (Polygon ir rs) = V.cons ir rs
{-# INLINE polygonRings #-}

instance VectorSpace v => HasCoordinates (Triangle v crs) [[Double]] where
  coordinates (Triangle a b c) = map coordinates [a, b, c, a]
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (TIN v crs) [[[Double]]] where
  coordinates = views triangles (V.toList . V.map coordinates . G.convert)
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (U.Vector (Point v crs)) [[Double]] where
  coordinates = V.toList . V.map coordinates . V.convert
  {-# INLINE coordinates #-}

instance VectorSpace v
  => HasCoordinates (V.Vector (Point v crs)) [[Double]] where
  coordinates = V.toList . V.map coordinates
  {-# INLINE coordinates #-}

-- | A feature of 'GeometryType' t, vertex type 'v' and associated data 'd'
data FeatureT (g :: (* -> *) -> Symbol -> *) v (crs::Symbol) d = Feature {
    _featureTGeometry   :: g v crs
  , _featureTProperties :: d
  } deriving (Eq, Show, Functor)
makeFields ''FeatureT

instance (NFData d, NFData (g v crs)) => NFData (FeatureT g v crs d) where
  rnf (Feature g v) = rnf g `seq` rnf v `seq` ()

type Feature = FeatureT Geometry

derivingUnbox "UnboxFeature"
    [t| forall g v crs a. (VectorSpace v, U.Unbox a, U.Unbox (g v crs))
        => FeatureT g v crs a -> (g v crs, a) |]
    [| \(Feature p v) -> (p,v) |]
    [| \(p,v) -> Feature p v|]

newtype FeatureCollectionT (g :: (* -> *) -> Symbol -> *) v (crs::Symbol) d
  = FeatureCollection {
    _featureCollectionTFeatures :: [FeatureT g v crs d]
  } deriving (Eq, Show, Functor)
makeFields ''FeatureCollectionT

type FeatureCollection = FeatureCollectionT Geometry

instance Monoid (FeatureCollectionT g v crs d) where
    mempty = FeatureCollection mempty
    (FeatureCollection as) `mappend` (FeatureCollection bs)
        = FeatureCollection $ as `mappend` bs



data SomeGeometry (g :: (* -> *) -> Symbol -> *) v
  = forall crs. KnownSymbol crs => SomeGeometry (g v crs)


instance Show (g v NoCrs) => Show (SomeGeometry g v) where
  show (SomeGeometry g) = show (unsafeCoerce g :: g v NoCrs)
  showsPrec i (SomeGeometry g) = showsPrec i (unsafeCoerce g :: g v NoCrs)

instance Eq (g v NoCrs) => Eq (SomeGeometry g v) where
  sa@(SomeGeometry a) == sb@(SomeGeometry b) =
    sameCrs sa sb &&
        (unsafeCoerce a :: g v NoCrs) == (unsafeCoerce b :: g v NoCrs)

instance HasVertex (SomeGeometry Point v) (Vertex v) where
  vertex = lens (\(SomeGeometry v) -> v^.vertex)
                (\(SomeGeometry v) a -> SomeGeometry (v & vertex .~ a))

data SomeFeatureT g v a
  = forall crs. KnownSymbol crs => SomeFeature (FeatureT g v crs a)

instance (Show (g v NoCrs), Show a) => Show (SomeFeatureT g v a) where
  show (SomeFeature g) = show (unsafeCoerce g :: FeatureT g v NoCrs a)
  showsPrec i (SomeFeature g)
    = showsPrec i (unsafeCoerce g :: FeatureT g v NoCrs a)

instance (Eq a, Eq (g v NoCrs)) => Eq (SomeFeatureT g v a) where
  sa@(SomeFeature a) == sb@(SomeFeature b) =
    sameCrs sa sb &&
        (unsafeCoerce a :: FeatureT g v NoCrs a) == (unsafeCoerce b :: FeatureT g v NoCrs a)

instance HasProperties (SomeFeatureT g v a) a where
  properties = lens (\(SomeFeature f) -> f^.properties)
                    (\(SomeFeature f) p -> SomeFeature (f & properties .~ p))

instance HasGeometry (SomeFeatureT g v a) (SomeGeometry g v) where
  geometry =
    lens (\(SomeFeature v) -> SomeGeometry (v^.geometry))
         (\(SomeFeature (Feature _ p)) (SomeGeometry g) ->
            SomeFeature (Feature g p))

data SomeFeatureCollectionT g v a
  = forall crs. KnownSymbol crs
  => SomeFeatureCollection (FeatureCollectionT g v crs a)

instance (Show (g v NoCrs), Show a)
  => Show (SomeFeatureCollectionT g v a) where
  show (SomeFeatureCollection g) =
    show (unsafeCoerce g :: FeatureCollectionT g v NoCrs a)
  showsPrec i (SomeFeatureCollection g)
    = showsPrec i (unsafeCoerce g :: FeatureCollectionT g v NoCrs a)

instance (Eq a, Eq (g v NoCrs)) => Eq (SomeFeatureCollectionT g v a) where
  sa@(SomeFeatureCollection a) == sb@(SomeFeatureCollection b) =
    sameCrs sa sb &&
        (unsafeCoerce a :: FeatureCollectionT g v NoCrs a) == (unsafeCoerce b :: FeatureCollectionT g v NoCrs a)

type SomeFeatureCollection v a = SomeFeatureCollectionT Geometry
type SomeFeature v a = SomeFeatureT Geometry

class HasSameCrs o where
  sameCrs :: o -> o -> Bool

instance HasSameCrs (SomeGeometry g v) where
  sameCrs (SomeGeometry (_ :: g v c1))
          (SomeGeometry (_ :: g v c2)) =
    case sameSymbol (Proxy :: Proxy c1) (Proxy :: Proxy c2) of
      Just _  -> True
      Nothing -> False

instance HasSameCrs (SomeFeatureT g v a) where
  sameCrs (SomeFeature (_ :: FeatureT g v c1 a))
          (SomeFeature (_ :: FeatureT g v c2 a)) =
    case sameSymbol (Proxy :: Proxy c1) (Proxy :: Proxy c2) of
      Just _  -> True
      Nothing -> False

instance HasSameCrs (SomeFeatureCollectionT g v a) where
  sameCrs (SomeFeatureCollection (_ :: FeatureCollectionT g v c1 a))
          (SomeFeatureCollection (_ :: FeatureCollectionT g v c2 a)) =
    case sameSymbol (Proxy :: Proxy c1) (Proxy :: Proxy c2) of
      Just _  -> True
      Nothing -> False

data Raster vs (t :: OffsetType) crs v a
  = Raster {
      rGeoReference :: !(GeoReference vs crs)
    , rData         :: !(v a)
    } deriving (Eq, Show)




rasterIndex
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Point vs crs -> Maybe Int
rasterIndex r p = fmap unOff offset
  where
    offset = pointOffset (rGeoReference r) p :: Maybe (Offset t)
{-# INLINE rasterIndex #-}

unsafeRasterIndex
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Point vs crs -> Int
unsafeRasterIndex r p = unOff offset
  where
    offset = unsafePointOffset (rGeoReference r) p :: Offset t
{-# INLINE unsafeRasterIndex #-}

rasterIndexPixel
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Pixel vs -> Maybe Int
rasterIndexPixel r px = fmap unOff offset
  where
    offset = toOffset (grSize (rGeoReference r)) px :: Maybe  (Offset t)
{-# INLINE rasterIndexPixel #-}

unsafeRasterIndexPixel
  :: forall vs t crs v a. (HasOffset vs t)
  => Raster vs t crs v a -> Pixel vs -> Int
unsafeRasterIndexPixel r px = unOff offset
  where
    offset = unsafeToOffset (grSize (rGeoReference r)) px :: Offset t
{-# INLINE unsafeRasterIndexPixel #-}


convertRasterOffsetType
  :: forall vs t1 t2 crs v a. (G.Vector v a, HasOffset vs t1, HasOffset vs t2)
  => Raster vs t1 crs v a -> Raster vs t2 crs v a
convertRasterOffsetType r = r {rData = G.generate n go}
  where go i = let px        = unsafeFromOffset s (Offset i :: Offset t2)
                   Offset i' = unsafeToOffset s px :: Offset t1
               in rd `G.unsafeIndex` i'
        s = grSize (rGeoReference r)
        rd = rData r
        n  = G.length rd
