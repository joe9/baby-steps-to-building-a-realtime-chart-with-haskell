{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module VolumeGraphOpenGL
  (volumeGraph)
  where

import           Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           "gl" Graphics.GL
import           Linear.V2
--
import OpenGLStuff
import ScaleUnboxedVector
import TypesOpenGL

-- TODO            map dot scaledVolumes)
volumeGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> VU.Vector PriceData -> Picture
volumeGraph xScale yScale dataSeries =
  Picture ((VS.concatMap v2ToVertex .
            VU.convert .
            VU.concatMap
              (\(V2 x y) ->
                 VU.fromList
                   [V2 (x - barWidthHalved)
                       (minRange yScale)
                   ,V2 (x - barWidthHalved) y
                   ,V2 (x + barWidthHalved)
                       (minRange yScale)
                   ,V2 (x + barWidthHalved) y]) .
            VU.imap (scaledVertex xScale yScale)) dataSeries)
          GL_TRIANGLE_STRIP
          lightgrey
          Nothing
  where chartWidth = maxRange xScale - minRange xScale
        barWidthHalved = (barWidth chartWidth (VU.length dataSeries)) / 2

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / (fromIntegral n)

-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledVertex
  :: (Scale xScale
     ,Scale yScale)
  => xScale -> yScale -> Int -> PriceData -> (V2 Double)
scaledVertex xScale yScale x =
  V2 ((toRange xScale . fromIntegral) x) . toRange yScale . volume
