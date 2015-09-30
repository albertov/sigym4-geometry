{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Sigym4.Geometry.QuadTree.Internal.TH (
   machineEpsilon
 , maxLevel
 , calculatedEpsilonAndLevel
) where

import Language.Haskell.TH.Syntax


calculatedEpsilonAndLevel :: (Int,Double)
calculatedEpsilonAndLevel = go 0 1
  where go !n !e | e+1>1     = go (n+1) (e*0.5)
                 | otherwise = (n,e)

machineEpsilon :: Int -> Q (TExp Double)
machineEpsilon f = let e = 2^f * (snd calculatedEpsilonAndLevel)
                   in [|| e ||]

maxLevel :: Int -> Q (TExp Int)
maxLevel f = let e = (fst calculatedEpsilonAndLevel) - f
             in [|| e ||]
