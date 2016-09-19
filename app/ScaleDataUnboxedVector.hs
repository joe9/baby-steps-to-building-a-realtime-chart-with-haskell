{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module ScaleDataUnboxedVector where

import qualified Data.Vector.Unboxed as VU

-- deinterpolate(a, b)(x) takes a domain value x in [a,b] and returns the corresponding parameter t in [0,1].
-- reinterpolate(a, b)(t) takes a parameter t in [0,1] and returns the corresponding domain value x in [a,b].
-- will have to change the Domain values from Double to something more generic
data Scale =
  Scale {sDomain      :: VU.Vector Double
        ,sMinRange    :: Double
        ,sMaxRange    :: Double
        ,sAddToDomain :: Scale -> Double -> Scale
        ,sToRange     :: Scale -> Double -> Double}

-- linearScale
--   :: VU.Vector Double -> Double -> Double -> Scale
-- linearScale ds mi ma =
--   Scale {sDomain = dom
--         ,sMinRange = mi
--         ,sMaxRange = ma
--         ,sAddToDomain = \s d -> (addToMinDomain d . addToMaxDomain d) s
--         ,sToRange = linearScaleDomainToRange}
--   where dom =
--           if VU.null ds
--              then VU.empty
--              else VU.cons (VU.minimum ds)
--                           (VU.singleton (VU.maximum ds))

linearScale
  :: Double -> Double -> Double -> Double -> Scale
linearScale dmin dmax rmin rmax =
  Scale {sDomain = VU.cons dmin (VU.singleton dmax)
        ,sMinRange = rmin
        ,sMaxRange = rmax
        ,sAddToDomain = \s d -> (addToMinDomain d . addToMaxDomain d) s
        ,sToRange = linearScaleDomainToRange}

addToMinDomain :: Double -> Scale -> Scale
addToMinDomain d s
  | VU.null dom = s {sDomain = VU.replicate 2 d}
  | otherwise =
    if VU.head dom > d
       then s {sDomain = VU.update dom (VU.singleton (0,d))}
       else s
  where dom = sDomain s

addToMaxDomain :: Double -> Scale -> Scale
addToMaxDomain d s
  | VU.null dom = s {sDomain = VU.replicate 2 d}
  | otherwise =
    case dom VU.!? 1 of
      Nothing -> s {sDomain = VU.update dom (VU.singleton (1,d))}
      (Just dold) ->
        if d > dold
           then s {sDomain = VU.update dom (VU.singleton (1,d))}
           else s
  where dom = sDomain s

linearScaleDomainToRange
  :: Scale -> Double -> Double
linearScaleDomainToRange scale domainValue =
  (((domainValue - minDomain) / (maxDomain - minDomain)) *
   (sMaxRange scale - sMinRange scale)) +
  sMinRange scale
  where minDomain = (VU.minimum . sDomain) scale
        maxDomain = (VU.maximum . sDomain) scale
