{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sigym4.Geometry.Arrow (
    FeatureArrow
  , mkPureFA
  , mkFA
  , runFA
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Applicative
import Control.Monad.Reader (Reader, MonadReader(ask), runReader)

import Sigym4.Geometry.Types (Geometry, Feature(_fGeom))

-- | A 'FeatureArrow' is an 'Arrow' that maps 'a's to 'b's which have an
--   associated read-only 'Geometry' of type 't' and vertex 'v'
newtype FeatureArrow t v a b = FeatureArrow (Kleisli (GeometryM t v) a b)

instance Arrow (FeatureArrow t v) where
    arr      = FeatureArrow . arr
    first (FeatureArrow (Kleisli f))
      = FeatureArrow $ Kleisli $ \(a,b) -> (,) <$> f a <*> pure b
    second (FeatureArrow (Kleisli f))
      = FeatureArrow $ Kleisli $ \(a,b) -> (,) <$> pure a <*> f b

instance Category (FeatureArrow t v) where
    (FeatureArrow g) . (FeatureArrow f) = FeatureArrow (g . f)
    id = arr id

type GeometryM t v = Reader (Geometry t v)

runGeometryM :: Geometry t v -> GeometryM t v a -> a
runGeometryM g gm = runReader gm g

-- | Constructs a 'FeatureArrow' from a pure function (a -> b)
mkPureFA :: (a -> b) -> FeatureArrow t v a b
mkPureFA = arr

-- | Constructs a 'FeatureArrow' from a function (Geometry t v -> a -> b)
--   This is used when the result value depends on the 'Geometry' of the
--   'Feature'
mkFA :: (Geometry t v -> a -> b) -> FeatureArrow t v a b
mkFA f = FeatureArrow $ Kleisli (\a -> f <$> ask <*> pure a)

-- | Runs a 'FeatureArrow' on the data of a 'Feature' using its 'Geometry'
--   as context and returns a 'Feature' with the same 'Geometry' and the
--   return value as data
runFA :: FeatureArrow t v a b -> Feature t v a -> Feature t v b
runFA (FeatureArrow f) feat
  = fmap (\v -> runGeometryM (_fGeom feat) (runKleisli f v)) feat
