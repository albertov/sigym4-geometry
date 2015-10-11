{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sigym4.Geometry.QuadTree.Internal.TH (
   machineEpsilon
 , machineEpsilonAndLevel
 , calculatedEpsilonAndLevel
) where

import Language.Haskell.TH.Syntax


calculatedEpsilonAndLevel :: (Ord a, Fractional a) => (Int,a)
calculatedEpsilonAndLevel = go 0 1
  where go !n !e | e+1>1     = go (n+1) (e*0.5)
                 | otherwise = (n,e)

machineEpsilon :: (Lift a, Ord a, Fractional a) => Int -> Q (TExp a)
machineEpsilon f = let e = 2^f * (snd calculatedEpsilonAndLevel)
                   in [|| e ||]

machineEpsilonAndLevel
  :: (Lift a, Ord a, Fractional a)
  => Int -> Q (TExp (Int,a))
machineEpsilonAndLevel f
  = let (l,e) = calculatedEpsilonAndLevel
        r     = (l-f, e * 2^f)
    in [|| r ||]

#if !MIN_VERSION_template_haskell(2,10,0)
instance Lift Double where
  lift x = return (LitE (RationalL (toRational x)))
#endif
