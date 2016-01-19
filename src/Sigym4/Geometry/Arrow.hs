{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, CPP #-}
module Sigym4.Geometry.Arrow (
    FeatureArrow
  , arr
  , mkFA
  , runFA
  , mapFA
) where

import Control.Arrow (Arrow, Kleisli(..), arr)
import Control.Category (Category)
import Control.Monad.Reader (Reader, MonadReader(ask), runReader)
import Control.Lens ((^.))

import Sigym4.Geometry.Types (Geometry, Feature, geometry)

-- | A 'FeatureArrow' is an 'Arrow' that maps 'a's to 'b's which have an
--   associated read-only 'Geometry' of type 't' and vertex 'v'
newtype FeatureArrow v a b = FeatureArrow (Kleisli (Reader (Geometry v)) a b)

deriving instance Arrow (FeatureArrow v)
deriving instance Category (FeatureArrow v)

-- | Constructs a 'FeatureArrow' from a function (Geometry v -> a -> b)
--   This is used when the result value depends on the 'Geometry' of the
--   'Feature'
mkFA :: (Geometry v -> a -> b) -> FeatureArrow v a b
mkFA f = FeatureArrow $ Kleisli (\a -> f <$> ask <*> pure a)

-- | Runs a 'FeatureArrow' on the data of a 'Feature' using its 'Geometry'
--   as context and returns a 'Feature' with the same 'Geometry' and the
--   return value as 'fData'
runFA :: FeatureArrow v a b -> Feature v a -> Feature v b
runFA (FeatureArrow (Kleisli f)) feat
  = fmap (\v -> runReader (f v) (feat^.geometry)) feat

-- | Maps a 'FeatureArrow' over a 'Functor'
mapFA :: Functor f => FeatureArrow v a b -> f (Feature v a) ->  f (Feature v b)
mapFA = fmap . runFA
