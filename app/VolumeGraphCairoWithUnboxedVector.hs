{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module VolumeGraphCairoWithUnboxedVector
  (volumeGraph)
  where

import Graphics.Rendering.Cairo        hiding (scale, x, y)
import Data.Colour.Names
import qualified Data.Vector.Unboxed as VU
import Graphics.Rendering.Cairo        hiding (scale,x,y)
import Data.Colour.SRGB
import Data.Colour.Names
import Linear.V2
--
import ScaleUnboxedVector
import TypesOpenGL

volumeGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> VU.Vector PriceData -> Render ()
volumeGraph xScale yScale dataSeries =
  VU.mapM_ (\(V2 x y) -> bar xScale yScale (barWidth chartWidth (VU.length dataSeries)) x y) scaledVertices >>
  VU.mapM_ dot scaledVertices
  where
        chartWidth = maxRange xScale - minRange xScale
        scaledVertices = VU.imap (scaledVertex xScale yScale) dataSeries

--   (showOrigin . position) (zip scaledVolumes (repeat dot) <> bars)
--   where bars =
--           map (uncurry (bar xScale yScale (barWidth chartWidth (length volumes)))) volumes


-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledVertex
  :: (Scale xScale
     ,Scale yScale)
  => xScale -> yScale -> Int -> PriceData -> (V2 Double)
scaledVertex xScale yScale x =
  V2 ((toRange xScale . fromIntegral) x) . toRange yScale . volume

type NumberOfEntry = Int

-- Assuming that the volume at the x-axis will always be 0,
-- when y = 0, volume is 0
bar :: (Scale xscale
       ,Scale yscale)
    => xscale
    -> yscale
    -> Width
    -> Double -- x scaled value
    -> Double -- y scaled value
    -> Render ()
bar xscale yscale barwidth xValue yValue = do
                useColor lightgrey
                rectangle
                     (xValue - (barwidth / 2)) -- x starting point
                     (minRange yscale) -- y starting ponit
                     barwidth -- width
                     yValue -- height
                fill

--   (p2 (toRange xscale (fromIntegral xValue),(toRange yscale yValue) / 2)
--   ,(showOrigin . vbar barwidth) (toRange yscale yValue - toRange yscale 0))
--   where vbar =
--           lineWidth ultraThin . fillColor lightgrey . lineWidth none . rect

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / fromIntegral n

  -- https://hackage.haskell.org/package/colour-2.3.3
  -- https://hackage.haskell.org/package/prizm-0.3.1.2
useRGB :: RGB Double -> Render ()
useRGB (RGB r b g) = setSourceRGB r b g

  -- http://www.w3.org/TR/SVG11/types.html#ColorKeywords
useColor :: Colour Double -> Render ()
useColor = useRGB . toSRGB

dot :: V2 Double -> Render ()
dot (V2 x y) = do
  useColor black
  arc x y 1 0 (2 * pi) -- 0.07
  fill
