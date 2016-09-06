{-# OPTIONS_GHC -fno-warn-partial-type-signatures   #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraph
  (priceGraph)
  where

import Diagrams.Prelude hiding (dot, frame)
--
import Scale

type Bid = (Int,Double)

type Ask = (Int,Double)

priceGraph
  :: (Scale xscale
     ,Scale yscale
     ,_)
  => xscale -> yscale -> [Bid] -> [Ask] -> QDiagram b V2 Double Any
priceGraph xScale yScale bids asks =
  (showOrigin . position)
    ([(head areaVertices,areaBetweenBidAndAsk areaVertices)] <>
     zip scaledBids (repeat dot) <>
     zip scaledAsks (repeat dot))
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
dot :: (_)
    => Diagram b
dot = (showOrigin . fillColor blue . circle) 1 -- 0.07

-- Overlay the dot on the above frame.
-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.
areaBetweenBidAndAsk
  :: (_)
  => [(P2 Double)] -> QDiagram b V2 Double Any
areaBetweenBidAndAsk =
  showOrigin .
  lineColor lightpink .
  fillColor lightpink . strokeLoop . closeLine . lineFromVertices
