{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module VolumeGraphCairo
  (volumeGraph)
  where

import Graphics.Rendering.Cairo        hiding (scale, x, y)
import Data.Colour.Names
--
import ScaleCairo
import Types hiding (Volume)

type Volume = (Int,Double)

volumeGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Volume] -> Render ()
volumeGraph xScale yScale volumes =
  mapM_ dot scaledVolumes
  >> mapM_ (uncurry (bar xScale yScale (barWidth chartWidth (length volumes)))) volumes
  where scaledVolumes = scaledPoints xScale yScale volumes
        chartWidth = maxRange xScale - minRange xScale

--   (showOrigin . position) (zip scaledVolumes (repeat dot) <> bars)
--   where bars =
--           map (uncurry (bar xScale yScale (barWidth chartWidth (length volumes)))) volumes

type NumberOfEntry = Int

-- Assuming that the volume at the x-axis will always be 0,
-- when y = 0, volume is 0
bar :: (Scale xscale
       ,Scale yscale)
    => xscale
    -> yscale
    -> Width
    -> NumberOfEntry
    -> Double
    -> Render ()
bar xscale yscale barwidth xValue yValue = do
                useColor lightgrey
                rectangle
                     (toRange xscale (fromIntegral xValue) - (barwidth / 2)) -- x starting point
                     (toRange yscale 0) -- y starting ponit
                     barwidth -- width
                     (toRange yscale yValue) -- height
                fill

--   (p2 (toRange xscale (fromIntegral xValue),(toRange yscale yValue) / 2)
--   ,(showOrigin . vbar barwidth) (toRange yscale yValue - toRange yscale 0))
--   where vbar =
--           lineWidth ultraThin . fillColor lightgrey . lineWidth none . rect

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / fromIntegral n
