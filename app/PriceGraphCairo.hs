{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraphCairo
  (priceGraph)
  where

import Graphics.Rendering.Cairo        hiding (scale, x, y)
import Linear.V2
--
import ScaleCairo

type Bid = (Int,Double)

type Ask = (Int,Double)

priceGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Bid] -> [Ask] -> Render ()
priceGraph xScale yScale bids asks =
    areaBetweenBidAndAsk areaVertices >>
     mapM_ dot scaledBids >>
     mapM_ dot scaledAsks
--      zip scaledAsks (repeat dot))
--      zip scaledAsks (repeat dot))
  where scaledBids = scaledPoints xScale yScale bids
        scaledAsks = scaledPoints xScale yScale asks
        areaVertices = scaledBids ++ reverse scaledAsks

--         chartWidth = maxRange xScale - minRange xScale
--         chartHeight = maxRange yScale - minRange yScale
-- Draw a single blue coloured dot showing the local origin.
-- Related conversation on #diagrams:
-- What am I doing wrong here? translate (2 ^& 3) == translateX 2 . translateY 3, correct?
-- The circle is drawn but is not moving away from the edge
-- <byorgey> joe9: coordinates in diagrams are always relative, and it recenters and resizes the diagram so it fits exactly in the output image
-- <byorgey> joe9: so  circle 1 # translate (2 ^& 3)  does indeed move the circle to the point (2,3), but then when it renders it, it centers the output on (2,3) since that's where the circle is
-- <byorgey> usually this is a good thing because it frees you from having to think about where things are, or how big they are
-- <byorgey> but I can see how it can be confusing if you want to make a chart.
-- <byorgey> I suggest making a "canvas" first by making a large rectangle of the size you want your background to be, then drawing stuff on top of that.  You can even make it aninvisible rectangle.
dot :: V2 Double -> Render ()
dot (V2 x y) = do
  setSourceRGB 1 1 0
  arc x y 1 0 0 -- 0.07
  fill

-- Overlay the dot on the above frame.
-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.
areaBetweenBidAndAsk :: [(V2 Double)] -> Render ()
areaBetweenBidAndAsk [] = return ()
areaBetweenBidAndAsk points@(p:_) = do
--   lineColor lightpink .
  setSourceRGB 1 1 0
  (\(V2 x y) -> moveTo x y) p
--   mapM_ (uncurry lineTo) points
  mapM_ (\(V2 x y) -> lineTo x y) points
  fill
