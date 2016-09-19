{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraphCairoWithUnboxedVector
  (priceGraph)
  where

import Graphics.Rendering.Cairo        hiding (scale, x, y)
import qualified Data.Vector.Unboxed as VU
import Data.Colour.Names
import Data.Colour.SRGB
import Linear.V2
--
import ScaleUnboxedVector
import TypesOpenGL

priceGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> VU.Vector PriceData -> Render ()
priceGraph xScale yScale dataSeries =
    areaBetweenBidAndAsk areaVertices >>
     VU.mapM_ dot scaledBids >>
     VU.mapM_ dot scaledAsks
--      zip scaledAsks (repeat dot))
--      zip scaledAsks (repeat dot))
  where scaledBids = VU.imap (scaledVertex bid xScale yScale) dataSeries
        scaledAsks = VU.imap (scaledVertex ask xScale yScale) dataSeries
        areaVertices = scaledBids VU.++ VU.reverse scaledAsks

-- Overlay the dot on the above frame.
-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.
areaBetweenBidAndAsk :: VU.Vector (V2 Double) -> Render ()
areaBetweenBidAndAsk points
  | VU.null points = return ()
  | otherwise = do
        let (V2 ix iy) = VU.head points
        useColor lightpink
        l <- getLineJoin
        setLineJoin LineJoinRound
        moveTo ix iy
        VU.mapM_ (\(V2 x y) -> lineTo x y) (VU.tail points)
        closePath
        fill
        setLineJoin l

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

-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledVertex
  :: (Scale xScale
     ,Scale yScale)
  => (PriceData -> Double) -> xScale -> yScale -> Int -> PriceData -> (V2 Double)
scaledVertex f xScale yScale x =
  V2 ((toRange xScale . fromIntegral) x) . toRange yScale . f
