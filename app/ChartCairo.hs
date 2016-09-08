{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module ChartCairo where

import Graphics.Rendering.Cairo        hiding (scale,x,y)
import Data.Colour.Names
--
-- import AxisCairo
import PriceGraphCairo
import ScaleV2
import Types
import VolumeGraphCairo

xScale, priceScale, volumeScale
  :: Width -> Height -> [(Int,Bid,Ask,Volume)] -> LinearScale
xScale w _ dataSeries =
  LinearScale (map (\(i,_,_,_) -> fromIntegral i) dataSeries)
              margin
              (chartWidth w)

priceScale _ h dataSeries =
  LinearScale (concatMap (\(_,b,a,_) -> [b,a]) dataSeries)
              (margin + volumeChartHeight h)
              (margin + volumeChartHeight h + priceChartHeight h)

volumeScale _ h dataSeries =
  LinearScale (map (\(_,_,_,v) -> v) dataSeries)
              margin
              (margin + volumeChartHeight h)

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
frame :: Width -> Height -> Render ()
frame w h = do
    useColor black

    moveTo 0 0
    rectangle 0 0 w h
    stroke

pChart
  :: Width -> Height -> [(Int,Bid,Ask,Volume)] -> Render ()
pChart w h dataSeries =
  priceGraph (xScale w h dataSeries)
              (priceScale w h dataSeries)
              (map (\(i,b,_,_) -> (i,b)) dataSeries)
              (map (\(i,_,a,_) -> (i,a)) dataSeries)

vChart
  :: Width -> Height -> [(Int,Bid,Ask,Volume)] -> Render ()
vChart w h dataSeries =
  volumeGraph (xScale w h dataSeries)
               (volumeScale w h dataSeries)
               (map (\(i,_,_,v) -> (i,v)) dataSeries)

chart
  :: Width -> Height -> [(Int,Bid,Ask,Volume)] -> Render ()
chart w h dataSeries =
  frame w h >> pChart w h dataSeries >> vChart w h dataSeries
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
margin = 20

chartWidth, chartHeight, priceChartHeight, volumeChartHeight
  :: Double -> Double
chartWidth w = w - (2 * margin)

chartHeight h = h - (2 * margin)

priceChartHeight = ( * 0.8) . chartHeight

volumeChartHeight =  (* 0.2) . chartHeight
