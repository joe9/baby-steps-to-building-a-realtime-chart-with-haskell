{-# LANGUAGE PackageImports #-}

module ChartOpenGL where

import           Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           "gl" Graphics.GL
--
import OpenGLStuff
import PriceGraphOpenGL
import ScaleUnboxedVector
import TypesOpenGL
import VolumeGraphOpenGL

minimumElement, maximumElement
  :: (Ord a
     ,VU.Unbox b)
  => (b -> a) -> VU.Vector b -> a
minimumElement f =
  f .
  VU.minimumBy
    (\a b ->
       compare (f a)
               (f b))

maximumElement f =
  f .
  VU.maximumBy
    (\a b ->
       compare (f a)
               (f b))

xScale, priceScale, volumeScale
  :: VU.Vector PriceData -> LinearScale
xScale dataSeries =
  LinearScale 0
              (fromIntegral (VU.length dataSeries - 1))
              (-1 + margin)
              (1 - margin)

priceScale dataSeries =
  LinearScale
    (min (minimumElement bid dataSeries)
         (minimumElement ask dataSeries))
    (max (maximumElement bid dataSeries)
         (maximumElement ask dataSeries))
    (-1 + margin + volumeChartHeight 2)
    (-1 + margin + volumeChartHeight 2 + priceChartHeight 2)

volumeScale dataSeries =
  LinearScale (minimumElement volume dataSeries)
              (maximumElement volume dataSeries)
              (-1 + margin)
              (-1 + margin + volumeChartHeight 2)

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
--   Picture [V2 (-1 :: Double) (-1), V2 (-1 :: Double)  1, V2  1  1, V2  1  (-1 :: Double)]
frame :: Picture
frame =
  Picture (VS.fromList [-0.99,-0.99,-0.99,0.99,0.99,0.99,0.99,-0.99]) GL_LINE_LOOP green Nothing

pChart
  :: (Scale x
     ,Scale y)
  => x -> y -> VU.Vector PriceData -> Picture
pChart xscale pricescale dataSeries = priceGraph xscale pricescale dataSeries

vChart
  :: (Scale x
     ,Scale y)
  => x -> y -> VU.Vector PriceData -> Picture
vChart xscale volumescale dataSeries =
  volumeGraph xscale volumescale dataSeries

chart
  :: (Scale x
     ,Scale priceScale
     ,Scale volumeScale)
  => x -> priceScale -> volumeScale -> VU.Vector PriceData -> [Picture]
chart x p v dataSeries = [frame,pChart x p dataSeries,vChart x v dataSeries]

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
