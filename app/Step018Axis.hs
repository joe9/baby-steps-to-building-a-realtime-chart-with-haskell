{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude             hiding (dot, frame)
--
import Axis
import Scale

data MyData =
  MyData {mdId     :: Int
         ,mdBid    :: Double
         ,mdAsk    :: Double
         ,mdVolume :: Double}
  deriving (Eq,Read,Show)

dataSeries :: [MyData]
dataSeries =
  [MyData 1 1.19 1.26 1000
  ,MyData 2 1.22 1.27 2000
  ,MyData 3 1.27 1.37 1000
  ,MyData 4 1.37 1.47 0
  ,MyData 5 1.67 1.97 3000
  ,MyData 6 1.57 1.67 1000
  ,MyData 7 1.47 1.57 1000
  ,MyData 8 1.27 1.37 500
  ,MyData 9 1.17 1.25 5000
  ,MyData 10 1.1 1.15 0]

xScale, priceScale, volumeScale :: LinearScale
xScale =
  LinearScale (map (fromIntegral . mdId) dataSeries)
              0
              chartWidth

priceScale =
  LinearScale (concatMap (\d -> [mdBid d,mdAsk d]) dataSeries)
              0
              priceChartHeight

volumeScale =
  LinearScale (map mdVolume dataSeries)
              0
              volumeChartHeight

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
frame :: QDiagram B V2 Double Any
frame = (showOrigin . lineWidth ultraThin . rect frameWidth) frameHeight

xAxis :: QDiagram B V2 Double Any
xAxis =
  position [(p2 (frameWidth / 2,frameHeight / 2),frame)
           ,(p2 (frameWidth / 2,margin),bottomAxis xScale)
           ,(p2 (frameWidth / 2,frameHeight - margin),topAxis xScale)
           ,(p2 (margin,frameHeight / 2),leftAxis xScale)
           ,(p2 (frameHeight - margin,frameHeight / 2),rightAxis xScale)]

--            ,(p2 (frameWidth/2,margin) , axis xScale)
-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.
margin, frameWidth, frameHeight :: Double
frameWidth = 500 + (2 * margin)

frameHeight = 500 + (2 * margin)

margin = 20

chartWidth, chartHeight, priceChartHeight, volumeChartHeight
  :: Double
chartWidth = 500

chartHeight = 500

priceChartHeight = 300

volumeChartHeight = 200

-- Compile using similar commands as in Step 1.
-- The mainWith translates the logical units used in w and h to the
--  --width and --height arguments (actual units) provided at the
--  command line.
main :: IO ()
main = mainWith xAxis
