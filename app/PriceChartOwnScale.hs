{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceChartOwnScale
  (priceChart)
  where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude             hiding (dot, frame)
--
import Scale

type Bid = (Int,Double)

type Ask = (Int,Double)

priceChart
  :: [Bid] -> [Ask] -> QDiagram B V2 Double Any
priceChart bids asks =
  (showOrigin . position)
    ([(p2 (chartWidth / 2,0),xAxis),(p2 (0,chartHeight / 2),yAxis)] <>
     [(head areaVertices,areaBetweenBidAndAsk areaVertices)] <>
     zip scaledBids (repeat dot) <>
     zip scaledAsks (repeat dot))
  where scaledBids = scaledPoints xScale yScale bids
        scaledAsks = scaledPoints xScale yScale asks
        xScale = LinearScale (map (fromIntegral . fst) (bids <> asks)) 0 chartWidth
        yScale = LinearScale (map snd (bids <> asks)) 0 chartHeight
        areaVertices = scaledBids ++ reverse scaledAsks

-- Draw a single blue coloured dot showing the local origin.
-- Related conversation on #diagrams:
-- What am I doing wrong here? translate (2 ^& 3) == translateX 2 . translateY 3, correct?
-- The circle is drawn but is not moving away from the edge
-- <byorgey> joe9: coordinates in diagrams are always relative, and it recenters and resizes the diagram so it fits exactly in the output image
-- <byorgey> joe9: so  circle 1 # translate (2 ^& 3)  does indeed move the circle to the point (2,3), but then when it renders it, it centers the output on (2,3) since that's where the circle is
-- <byorgey> usually this is a good thing because it frees you from having to think about where things are, or how big they are
-- <byorgey> but I can see how it can be confusing if you want to make a chart.
-- <byorgey> I suggest making a "canvas" first by making a large rectangle of the size you want your background to be, then drawing stuff on top of that.  You can even make it aninvisible rectangle.
dot :: Diagram B
dot = (showOrigin . fillColor blue . circle) 1 -- 0.07

xAxis, yAxis :: QDiagram B V2 Double Any
-- xAxis = (showOrigin . lineWidth veryThin . fromVertices) [p2 (0,0),p2 (chartWidth,0)]
xAxis = (showOrigin . lineWidth veryThin . hrule) chartWidth

yAxis = (showOrigin . lineWidth veryThin . vrule) chartHeight

-- Overlay the dot on the above frame.
-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.
areaBetweenBidAndAsk
  :: [(P2 Double)] -> QDiagram B V2 Double Any
areaBetweenBidAndAsk =
  showOrigin .
  lineColor lightpink .
  fillColor lightpink . strokeLoop . closeLine . lineFromVertices

chartWidth :: Double
chartWidth = 100

chartHeight :: Double
chartHeight = 100
