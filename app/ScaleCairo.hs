{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module ScaleCairo where

import Linear.V2

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

-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledPoints
  :: (Scale x
     ,Scale y)
  => x -> y -> [(Int,Double)] -> [(V2 Double)]
scaledPoints xScale yScale =
  map (\(i,v) -> V2 ( (toRange xScale . fromIntegral) i) ( (toRange yScale) v))

data LinearScale =
  LinearScale {sDomain   :: [Double]
              ,sMinRange :: Double
              ,sMaxRange :: Double}

linearScaleDomainToRange
  :: LinearScale -> Double -> Double
linearScaleDomainToRange scale domainValue =
  (((domainValue - minDomain scale) / (maxDomain scale - minDomain scale)) *
   (maxRange scale - minRange scale)) +
  minRange scale

minDomain, maxDomain :: LinearScale -> Double
minDomain = minimum . sDomain

maxDomain = maximum . sDomain

linearScaleRangeToDomain
  :: LinearScale -> Double -> Double
linearScaleRangeToDomain scale rangeValue =
  (((rangeValue - minRange scale) / (maxRange scale - minRange scale)) *
   (maxDomain scale - minDomain scale)) +
  minDomain scale

instance Scale LinearScale where
  domain = sDomain
  toRange = linearScaleDomainToRange
  toDomain = linearScaleRangeToDomain
  minRange = sMinRange
  maxRange = sMaxRange
