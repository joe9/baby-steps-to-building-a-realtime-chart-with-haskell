{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module ChartOpenGL where

import Data.Colour.Names
import Data.Monoid
import Data.Ord
import Data.Tuple.Select
import "gl" Graphics.GL
import Linear.V2
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
-- import AxisCairo
import OpenGLStuff
import PriceGraphOpenGL
import ScaleUnboxedVector
import TypesOpenGL
import VolumeGraphOpenGL

xScale, priceScale, volumeScale :: VU.Vector PriceData -> LinearScale
xScale dataSeries =
  LinearScale
              (fromIntegral (VU.minIndex dataSeries))
              (fromIntegral (VU.maxIndex dataSeries))
              (-1 + margin)
              (1 - margin)

priceScale dataSeries =
  LinearScale
              (VU.foldl' (\a p -> min ((min a . bid) p) (ask p)) 0 dataSeries)
              (VU.foldl' (\a p -> max ((max a . bid) p) (ask p)) 0 dataSeries)
              (-1 + margin + volumeChartHeight 2)
              (-1 + margin + volumeChartHeight 2 + priceChartHeight 2)

volumeScale dataSeries =
  LinearScale
              (VU.foldl' (\a p -> (min a . volume) p) 0 dataSeries)
              (VU.foldl' (\a p -> (max a . volume) p) 0 dataSeries)
              (-1 + margin)
              (-1 + margin + volumeChartHeight 2)

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
--   Picture [V2 (-1 :: Double) (-1), V2 (-1 :: Double)  1, V2  1  1, V2  1  (-1 :: Double)]
frame :: Picture
frame =
  Picture (VS.fromList [(-0.99),(-0.99)
          ,(-0.99),0.99
          ,0.99,0.99
          ,0.99,(-0.99)])
          GL_LINE_LOOP
          green
          Nothing

pChart
  :: (Scale x, Scale y) => x -> y -> VU.Vector PriceData -> Picture
pChart xScale priceScale dataSeries = priceGraph xScale priceScale dataSeries


vChart
  :: (Scale x, Scale y) => x -> y -> VU.Vector PriceData -> Picture
vChart xScale volumeScale dataSeries =
  volumeGraph xScale volumeScale dataSeries

chart
  :: (Scale x, Scale priceScale, Scale volumeScale) => x -> priceScale -> volumeScale -> VU.Vector PriceData -> [ Picture]
-- 16 milliseconds
-- chart w h dataSeries = [frame w h]
chart x p v dataSeries = [frame , pChart x p dataSeries , vChart x v dataSeries]

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
