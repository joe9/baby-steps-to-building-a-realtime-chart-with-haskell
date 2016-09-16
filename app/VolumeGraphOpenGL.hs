{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module VolumeGraphOpenGL
  (volumeGraph)
  where

import Data.Colour.Names
import Data.Monoid
import "gl" Graphics.GL
import Linear.V2
--
import OpenGLStuff
import ScaleV2
import TypesOpenGL hiding (Volume)

type Volume = (Int,Double)

-- TODO            map dot scaledVolumes)
volumeGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Volume] -> Picture
volumeGraph xScale yScale volumes =
  Picture ([startingPoint] <>
           concatMap (\(V2 x y) ->
                        [V2 (x - barWidthHalved)
                            (minRange yScale)
                        ,V2 (x - barWidthHalved) y
                        ,V2 (x + barWidthHalved)
                            (minRange yScale)
                        ,V2 (x + barWidthHalved) y])
                     scaledVolumes <>
           [endingPoint])
          GL_TRIANGLE_STRIP
          lightgrey
          Nothing
  where scaledVolumes = scaledPoints xScale yScale volumes
        chartWidth = maxRange xScale - minRange xScale
        barWidthHalved = (barWidth chartWidth (length volumes)) / 2
        startingPoint =
          V2 (minRange xScale - barWidthHalved)
             (minRange yScale)
        endingPoint =
          V2 (maxRange xScale + barWidthHalved)
             (minRange yScale)

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / (fromIntegral n)
