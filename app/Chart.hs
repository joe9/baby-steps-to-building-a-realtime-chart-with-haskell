{-# OPTIONS_GHC -fno-warn-partial-type-signatures     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module Chart where

import Diagrams.Prelude hiding (dot, frame, set)
--
import Axis
import PriceGraph
import Scale
import VolumeGraph

type Bid = Double

type Ask = Double

type Volume = Double

xScale, priceScale, volumeScale
  :: [(Int,Bid,Ask,Volume)] -> LinearScale
xScale dataSeries =
  LinearScale (map (\(i,_,_,_) -> fromIntegral i) dataSeries)
              0
              chartWidth

priceScale dataSeries =
  LinearScale (concatMap (\(_,b,a,_) -> [b,a]) dataSeries)
              0
              priceChartHeight

volumeScale dataSeries =
  LinearScale (map (\(_,_,_,v) -> v) dataSeries)
              0
              volumeChartHeight

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
frame :: _
      => QDiagram b V2 Double Any
frame = (showOrigin . lineWidth ultraThin . rect frameWidth) frameHeight

pChart
  :: _
  => [(Int,Bid,Ask,Volume)] -> QDiagram b V2 Double Any
pChart dataSeries =
  --   showEnvelope
  (priceGraph (xScale dataSeries)
              (priceScale dataSeries)
              (map (\(i,b,_,_) -> (i,b)) dataSeries)
              (map (\(i,_,a,_) -> (i,a)) dataSeries))

vChart
  :: _
  => [(Int,Bid,Ask,Volume)] -> QDiagram b V2 Double Any
vChart dataSeries =
  --   showEnvelope
  (volumeGraph (xScale dataSeries)
               (volumeScale dataSeries)
               (map (\(i,_,_,v) -> (i,v)) dataSeries))

chart
  :: _
  => [(Int,Bid,Ask,Volume)] -> QDiagram b V2 Double Any
chart dataSeries =
  position [(p2 (frameWidth / 2,frameHeight / 2),frame)
           ,(p2 (margin,margin + volumeChartHeight),pChart dataSeries)
           ,(p2 (margin,margin),vChart dataSeries)
           ,(p2 (frameWidth / 2,margin),bottomAxis (xScale dataSeries))
           ,(p2 (frameWidth / 2,margin + volumeChartHeight)
            ,bottomAxis (xScale dataSeries))
           ,(p2 (frameWidth / 2,frameHeight - margin)
            ,topAxis (xScale dataSeries))
           ,(p2 (margin,margin + volumeChartHeight + (priceChartHeight / 2))
            ,leftAxis (priceScale dataSeries))
           ,(p2 (margin,margin + (volumeChartHeight / 2))
            ,leftAxis (volumeScale dataSeries))
           ,(p2 (frameWidth - margin
                ,margin + volumeChartHeight + (priceChartHeight / 2))
            ,rightAxis (priceScale dataSeries))
           ,(p2 (frameWidth - margin,margin + (volumeChartHeight / 2))
            ,rightAxis (volumeScale dataSeries))]

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
