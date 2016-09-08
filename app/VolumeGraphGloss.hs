{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module VolumeGraphGloss
  (volumeGraph)
  where

import Data.Colour.Names
import Data.Monoid
import Graphics.Gloss
--
import ScaleV2
import TypesGloss hiding (Volume)

type Volume = (Int,Double)

volumeGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Volume] -> Picture
volumeGraph xScale yScale volumes =
  Pictures (map (uncurry (bar xScale yScale (barWidth chartWidth (length volumes)))) volumes <>
            map dot scaledVolumes)
  where scaledVolumes = scaledPoints xScale yScale volumes
        chartWidth = maxRange xScale - minRange xScale

type NumberOfEntry = Int

-- Assuming that the volume at the x-axis will always be 0,
-- when y = 0, volume is 0
bar
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> Width -> NumberOfEntry -> Double -> Picture
bar xscale yscale barwidth xValue yValue =
  color (useColor lightgrey) .
  scale (rf (toRange xscale (fromIntegral xValue) - (barwidth / 2)))
        (rf (toRange yscale 0)) $ -- y starting ponit
  rectangleSolid (rf barwidth)
                 (rf (toRange yscale yValue)) -- height

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / fromIntegral n
