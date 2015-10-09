{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sigym4.Geometry.QuadTree.Internal.TH (
   machineEpsilon
 , calculatedEpsilon
) where

import Language.Haskell.TH.Syntax


calculatedEpsilon :: (Ord a, Fractional a) => a -> a
calculatedEpsilon s = go s
  where go !e | e+s>s     = go (e*0.5)
              | otherwise = e

machineEpsilon :: (Lift a, Ord a, Fractional a) => a -> Int -> Q (TExp a)
machineEpsilon s f = let e = 2^f * calculatedEpsilon s
                     in [|| e ||]
