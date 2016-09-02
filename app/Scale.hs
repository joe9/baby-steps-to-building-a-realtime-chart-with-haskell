{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Scale where

-- deinterpolate(a, b)(x) takes a domain value x in [a,b] and returns the corresponding parameter t in [0,1].
-- reinterpolate(a, b)(t) takes a parameter t in [0,1] and returns the corresponding domain value x in [a,b].
-- will have to change the Domain values from Double to something more generic
class Scale s  where
  domain :: s -> [Double]
  toRange :: s -> Double -> Double
  toDomain :: s -> Double -> Double
  minRange :: s -> Double
  minRange = fst . range
  maxRange :: s -> Double
  maxRange = snd . range
  range :: s -> (Double,Double)
  range s = (minRange s,maxRange s)

-- Assume Range is always 0 thru 1
data LinearScale =
  LinearScale {sDomain   :: [Double]
              ,sMinRange :: Double
              ,sMaxRange :: Double}

linearScaleDomainToRange
  :: LinearScale -> Double -> Double
linearScaleDomainToRange scale domainValue =
  ((domainValue - minDomain scale) / (maxDomain scale - minDomain scale) *
   (maxRange scale - minRange scale)) +
  minRange scale

minDomain, maxDomain :: LinearScale -> Double
minDomain = minimum . sDomain

maxDomain = maximum . sDomain

linearScaleRangeToDomain
  :: LinearScale -> Double -> Double
linearScaleRangeToDomain scale rangeValue =
  rangeValue * (maxDomain scale - minDomain scale)

instance Scale LinearScale where
  domain = sDomain
  toRange = linearScaleDomainToRange
  toDomain = linearScaleRangeToDomain
  minRange = sMinRange
  maxRange = sMaxRange
