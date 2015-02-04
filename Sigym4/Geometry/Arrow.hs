{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, CPP #-}
module Sigym4.Geometry.Arrow (
    FeatureArrow
  , arr -- ^ Constructs a 'FeatureArrow' from a pure function (a -> b)
  , mkFA
  , runFA
  , mapFA
) where

import Control.Arrow (Arrow, Kleisli(..), arr)
import Control.Category (Category)
import Control.Monad.Reader (Reader, MonadReader(ask), runReader)
#if MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#else
import Control.Applicative (pure, (<$>), (<*>))
#endif

import Sigym4.Geometry.Types (Geometry, Feature(_fGeom))

-- | A 'FeatureArrow' is an 'Arrow' that maps 'a's to 'b's which have an
--   associated read-only 'Geometry' of type 't' and vertex 'v'
newtype FeatureArrow t v a b = FeatureArrow (Kleisli (Reader (Geometry t v)) a b)

deriving instance Arrow (FeatureArrow t v)
deriving instance Category (FeatureArrow t v)

-- | Constructs a 'FeatureArrow' from a function (Geometry t v -> a -> b)
--   This is used when the result value depends on the 'Geometry' of the
--   'Feature'
mkFA :: (Geometry t v -> a -> b) -> FeatureArrow t v a b
mkFA f = FeatureArrow $ Kleisli (\a -> f <$> ask <*> pure a)

-- | Runs a 'FeatureArrow' on the data of a 'Feature' using its 'Geometry'
--   as context and returns a 'Feature' with the same 'Geometry' and the
--   return value as 'fData'
runFA :: FeatureArrow t v a b -> Feature t v a -> Feature t v b
runFA (FeatureArrow (Kleisli f)) feat
  = fmap (\v -> runReader (f v) (_fGeom feat)) feat

-- | Maps a 'FeatureArrow' over a 'Functor'
mapFA :: Functor f => FeatureArrow t v a b -> f (Feature t v a) ->  f (Feature t v b)
mapFA = fmap . runFA 
