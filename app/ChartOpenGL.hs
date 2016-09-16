{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module ChartOpenGL where

import Data.Colour.Names
import Data.Monoid
import "gl" Graphics.GL
import Linear.V2
-- import AxisCairo
import OpenGLStuff
import PriceGraphOpenGL
import ScaleV2
import TypesOpenGL
import VolumeGraphOpenGL

xScale, priceScale, volumeScale ::
                                [(Int,Bid,Ask,Volume)]
                                -> LinearScale
xScale dataSeries =
  LinearScale (map (\(i,_,_,_) -> fromIntegral i) dataSeries)
              (-1 + margin)
              (1 - margin)

priceScale dataSeries =
  LinearScale (concatMap (\(_,b,a,_) -> [b,a]) dataSeries)
              (-1 + margin + volumeChartHeight 2)
              (-1 + margin + volumeChartHeight 2 + priceChartHeight 2)

volumeScale dataSeries =
  LinearScale (map (\(_,_,_,v) -> v) dataSeries)
              (-1 + margin)
              (-1 + margin + volumeChartHeight 2)

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
--   Picture [V2 (-1 :: Double) (-1), V2 (-1 :: Double)  1, V2  1  1, V2  1  (-1 :: Double)]
frame :: Picture
frame =
  Picture [V2 (-0.99 :: Double)
              (-0.99)
          ,V2 (-0.99 :: Double) 0.99
          ,V2 0.99 0.99
          ,V2 0.99 (-0.99 :: Double)]
          GL_LINE_LOOP
          green
          Nothing

pChart
  :: [(Int,Bid,Ask,Volume)] -> Picture
pChart dataSeries =
  priceGraph (xScale dataSeries)
             (priceScale dataSeries)
             (map (\(i,b,a,_) -> (i,b,a)) dataSeries)


vChart
  :: [(Int,Bid,Ask,Volume)] -> Picture
vChart dataSeries =
  volumeGraph (xScale dataSeries)
              (volumeScale dataSeries)
              (map (\(i,_,_,v) -> (i,v)) dataSeries)

chart
  :: [(Int,Bid,Ask,Volume)] -> [Picture]
-- 16 milliseconds
-- chart w h dataSeries = [frame w h]
chart dataSeries = [frame , pChart dataSeries , vChart dataSeries]

--   position [(p2 (frameWidth / 2,frameHeight / 2),frame)
--            ,(p2 (margin,margin + volumeChartHeight),pChart dataSeries)
--            ,(p2 (margin,margin),vChart dataSeries)
--            ,(p2 (frameWidth / 2,margin),bottomAxis (xScale dataSeries))
--            ,(p2 (frameWidth / 2,margin + volumeChartHeight)
--             ,bottomAxis (xScale dataSeries))
--            ,(p2 (frameWidth / 2,frameHeight - margin)
--             ,topAxis (xScale dataSeries))
--            ,(p2 (margin,margin + volumeChartHeight + (priceChartHeight / 2))
--             ,leftAxis (priceScale dataSeries))
--            ,(p2 (margin,margin + (volumeChartHeight / 2))
--             ,leftAxis (volumeScale dataSeries))
--            ,(p2 (frameWidth - margin
--                 ,margin + volumeChartHeight + (priceChartHeight / 2))
--             ,rightAxis (priceScale dataSeries))
--            ,(p2 (frameWidth - margin,margin + (volumeChartHeight / 2))
--             ,rightAxis (volumeScale dataSeries))]
-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.
margin :: Double
margin = 0.05

chartWidth, chartHeight, priceChartHeight, volumeChartHeight
  :: Double -> Double
chartWidth w = w - (2 * margin)

chartHeight h = h - (2 * margin)

priceChartHeight = (* 0.8) . chartHeight

volumeChartHeight = (* 0.2) . chartHeight
