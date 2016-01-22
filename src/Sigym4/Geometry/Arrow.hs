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

import Sigym4.Geometry.Types (Feature(..))

-- | A 'FeatureArrow' is an 'Arrow' that maps 'a's to 'b's which have an
--   associated read-only 'Geometry' of type 't' and vertex 'v'
newtype FeatureArrow g crs a b =
  FeatureArrow (Kleisli (Reader (g crs)) a b)

deriving instance Arrow (FeatureArrow g crs)
deriving instance Category (FeatureArrow f crs)

-- | Constructs a 'FeatureArrow' from a function (Geometry t v -> a -> b)
--   This is used when the result value depends on the 'Geometry' of the
--   'Feature'
mkFA :: (g crs -> a -> b) -> FeatureArrow g crs a b
mkFA f = FeatureArrow $ Kleisli (\a -> f <$> ask <*> pure a)
{-# INLINE mkFA #-}

-- | Runs a 'FeatureArrow' on the data of a 'Feature' using its 'Geometry'
--   as context and returns a 'Feature' with the same 'Geometry' and the
--   return value as 'fData'
runFA :: FeatureArrow g crs a b -> Feature g a crs -> Feature g b crs
runFA (FeatureArrow (Kleisli f)) (Feature g p)
  = Feature g (runReader (f p) g)
{-# INLINE runFA #-}

-- | Maps a 'FeatureArrow' over a 'Functor'
mapFA :: Functor f => FeatureArrow g crs a b -> f (Feature g a crs) ->  f (Feature g b crs)
mapFA = fmap . runFA
{-# INLINE mapFA #-}
