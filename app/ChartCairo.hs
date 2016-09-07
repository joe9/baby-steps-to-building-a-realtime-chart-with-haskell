{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module ChartCairo where

import Graphics.Rendering.Cairo        hiding (scale)
--
-- import AxisCairo
import PriceGraphCairo
import ScaleCairo
import Types
import VolumeGraphCairo

xScale, priceScale, volumeScale
  :: [(Int,Bid,Ask,Volume)] -> LinearScale
xScale dataSeries =
  LinearScale (map (\(i,_,_,_) -> fromIntegral i) dataSeries)
              margin
              ( margin + chartWidth)

priceScale dataSeries =
  LinearScale (concatMap (\(_,b,a,_) -> [b,a]) dataSeries)
              (margin + volumeChartHeight)
              (margin + volumeChartHeight + priceChartHeight)

volumeScale dataSeries =
  LinearScale (map (\(_,_,_,v) -> v) dataSeries)
              margin
              (margin + volumeChartHeight)

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
frame :: Width -> Height -> Render ()
frame w h = do
    setSourceRGB 1 1 0
    setLineWidth 5

    moveTo 0 0
    lineTo w 0
    lineTo w h
    lineTo 0 h
    closePath
    stroke

pChart
  :: [(Int,Bid,Ask,Volume)] -> Render ()
pChart dataSeries =
  priceGraph (xScale dataSeries)
              (priceScale dataSeries)
              (map (\(i,b,_,_) -> (i,b)) dataSeries)
              (map (\(i,_,a,_) -> (i,a)) dataSeries)

vChart
  :: [(Int,Bid,Ask,Volume)] -> Render ()
vChart dataSeries =
  volumeGraph (xScale dataSeries)
               (volumeScale dataSeries)
               (map (\(i,_,_,v) -> (i,v)) dataSeries)

chart
  :: Width -> Height -> [(Int,Bid,Ask,Volume)] -> Render ()
chart w h dataSeries =
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
  frame w h >> pChart dataSeries >> vChart dataSeries

-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.
margin, frameWidth, frameHeight :: Double
frameWidth = 1000 + (2 * margin)

frameHeight = 1000 + (2 * margin)

margin = 20

chartWidth, chartHeight, priceChartHeight, volumeChartHeight
  :: Double
chartWidth = 1000

chartHeight = 1000

priceChartHeight = 800

volumeChartHeight = 200
