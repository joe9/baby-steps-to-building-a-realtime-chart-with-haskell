{-# OPTIONS_GHC -fno-warn-partial-type-signatures  #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module VolumeGraph
  (volumeGraph)
  where

import Diagrams.Prelude hiding (dot, frame)
--
import Scale

type Volume = (Int,Double)

volumeGraph
  :: (Scale xscale
     ,Scale yscale
     ,_)
  => xscale -> yscale -> [Volume] -> QDiagram _ V2 Double Any
volumeGraph xScale yScale volumes =
  (showOrigin . position) (zip scaledVolumes (repeat dot) <> bars)
  where bars =
          map (uncurry (bar xScale yScale (barWidth chartWidth (length volumes)))) volumes
        scaledVolumes = scaledPoints xScale yScale volumes
        chartWidth = maxRange xScale - minRange xScale

--         chartHeight = maxRange yScale - minRange yScale
-- type Low = Double
type High = Double

type Width = Double

type NumberOfEntry = Int

-- Assuming that the volume at the x-axis will always be 0,
-- when y = 0, volume is 0
bar :: (Scale xscale
       ,Scale yscale
       ,_)
    => xscale
    -> yscale
    -> Width
    -> NumberOfEntry
    -> High
    -> (Point V2 Double,QDiagram _ V2 Double Any)
bar xscale yscale barwidth xValue yValue =
  (p2 (toRange xscale (fromIntegral xValue),(toRange yscale yValue) / 2)
  ,(showOrigin . vbar barwidth) (toRange yscale yValue - toRange yscale 0))
  where vbar =
          lineWidth ultraThin . fillColor lightgrey . lineWidth none . rect

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / fromIntegral n

-- Draw a single blue coloured dot showing the local origin.
-- Related conversation on #diagrams:
-- What am I doing wrong here? translate (2 ^& 3) == translateX 2 . translateY 3, correct?
-- The circle is drawn but is not moving away from the edge
-- <byorgey> joe9: coordinates in diagrams are always relative, and it recenters and resizes the diagram so it fits exactly in the output image
-- <byorgey> joe9: so  circle 1 # translate (2 ^& 3)  does indeed move the circle to the point (2,3), but then when it renders it, it centers the output on (2,3) since that's where the circle is
-- <byorgey> usually this is a good thing because it frees you from having to think about where things are, or how big they are
-- <byorgey> but I can see how it can be confusing if you want to make a chart.
-- <byorgey> I suggest making a "canvas" first by making a large rectangle of the size you want your background to be, then drawing stuff on top of that.  You can even make it aninvisible rectangle.
dot :: (_)
    => Diagram b
dot = (showOrigin . fillColor blue . circle) 0.07
