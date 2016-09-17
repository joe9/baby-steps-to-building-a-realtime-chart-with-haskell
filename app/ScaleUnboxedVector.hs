{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module ScaleUnboxedVector where

import qualified Data.Vector.Unboxed as VU

-- deinterpolate(a, b)(x) takes a domain value x in [a,b] and returns the corresponding parameter t in [0,1].
-- reinterpolate(a, b)(t) takes a parameter t in [0,1] and returns the corresponding domain value x in [a,b].
-- will have to change the Domain values from Double to something more generic
class Scale s  where
--   domain :: s -> [Double]
  addToDomain :: s -> Double -> s
  toRange :: s -> Double -> Double
  toDomain :: s -> Double -> Double
  minRange :: s -> Double
  minRange = fst . range
  maxRange :: s -> Double
  maxRange = snd . range
  range :: s -> (Double,Double)
  range s = (minRange s,maxRange s)

data LinearScale =
  LinearScale {
               sMinDomain :: Double
              ,sMaxDomain :: Double
              ,sMinRange :: Double
              ,sMaxRange :: Double
              }

linearScaleDomainToRange
  :: LinearScale -> Double -> Double
linearScaleDomainToRange scale domainValue =
  (((domainValue - minDomain scale) / (maxDomain scale - minDomain scale)) *
   (maxRange scale - minRange scale)) +
  minRange scale

minDomain, maxDomain :: LinearScale -> Double
minDomain = sMinDomain

maxDomain = sMaxDomain

linearScaleRangeToDomain
  :: LinearScale -> Double -> Double
linearScaleRangeToDomain scale rangeValue =
  (((rangeValue - minRange scale) / (maxRange scale - minRange scale)) *
   (maxDomain scale - minDomain scale)) +
  minDomain scale

instance Scale LinearScale where
  toRange = linearScaleDomainToRange
  toDomain = linearScaleRangeToDomain
  minRange = sMinRange
  maxRange = sMaxRange
  addToDomain s d = (addToMinDomain d . addToMaxDomain d) s

addToMinDomain :: Double -> LinearScale -> LinearScale
addToMinDomain d s = if sMinDomain s > d
                        then s {sMinDomain = d}
                        else s

addToMaxDomain :: Double -> LinearScale -> LinearScale
addToMaxDomain d s = if d > sMaxDomain s
                        then s {sMaxDomain = d}
                        else s
