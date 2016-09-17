{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module ChartCairoWithUnboxedVector where

import Graphics.Rendering.Cairo        hiding (scale,x,y)
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Data.Vector.Unboxed as VU
--
-- import AxisCairo
import PriceGraphCairoWithUnboxedVector
import ScaleUnboxedVector
import TypesOpenGL
import VolumeGraphCairoWithUnboxedVector

xScale, priceScale, volumeScale
  :: Width -> Height -> VU.Vector PriceData -> LinearScale
xScale w _ dataSeries =
  LinearScale
              (fromIntegral (VU.minIndex dataSeries))
              (fromIntegral (VU.maxIndex dataSeries))
              margin
              (chartWidth w)

priceScale _ h dataSeries =
  LinearScale
              (VU.foldl' (\a p -> min ((min a . bid) p) (ask p)) 0 dataSeries)
              (VU.foldl' (\a p -> max ((max a . bid) p) (ask p)) 0 dataSeries)
              (margin + volumeChartHeight h)
              (margin + volumeChartHeight h + priceChartHeight h)

volumeScale _ h dataSeries =
  LinearScale
              (VU.foldl' (\a p -> (min a . volume) p) 0 dataSeries)
              (VU.foldl' (\a p -> (max a . volume) p) 0 dataSeries)
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
  :: Width -> Height -> VU.Vector PriceData -> Render ()
pChart w h dataSeries =
  priceGraph (xScale w h dataSeries)
              (priceScale w h dataSeries)
              dataSeries

vChart
  :: Width -> Height -> VU.Vector PriceData -> Render ()
vChart w h dataSeries =
  volumeGraph (xScale w h dataSeries)
               (volumeScale w h dataSeries)
               dataSeries

chart
  :: Width -> Height -> VU.Vector PriceData -> Render ()
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

  -- https://hackage.haskell.org/package/colour-2.3.3
  -- https://hackage.haskell.org/package/prizm-0.3.1.2
useRGB :: RGB Double -> Render ()
useRGB (RGB r b g) = setSourceRGB r b g

  -- http://www.w3.org/TR/SVG11/types.html#ColorKeywords
useColor :: Colour Double -> Render ()
useColor = useRGB . toSRGB
