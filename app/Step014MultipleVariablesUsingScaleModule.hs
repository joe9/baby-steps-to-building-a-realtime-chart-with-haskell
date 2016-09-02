{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (dot,frame)
--
import Scale

data MyData = MyData {mdId :: Int
                     ,mdBid :: Double
                     ,mdAsk :: Double
                     ,mdVolume :: Double
                     } deriving (Eq, Read, Show)

dataSeries :: [MyData]
dataSeries = [ MyData 1 1.19 1.26 1000
             , MyData 2 1.22 1.27 2000
             , MyData 3 1.27 1.37 1000
             , MyData 4 1.37 1.47 0
             , MyData 5 1.67 1.97 3000
             , MyData 6 1.57 1.67 1000
             , MyData 7 1.47 1.57 1000
             , MyData 8 1.27 1.37  500
             , MyData 9 1.17 1.25 5000
             , MyData 10 1.1 1.15 0
             ]

xScale, yScale :: LinearScale
xScale = LinearScale (map ((fromIntegral :: Int -> Double) . mdId) dataSeries) 0 chartWidth
yScale = LinearScale allBidsAndAsks 0 chartHeight

allBidsAndAsks :: [Double]
allBidsAndAsks = concatMap (\a -> [mdBid a,mdAsk a]) dataSeries

scaledBids, scaledAsks :: [(Double,Double)]
scaledBids = map (\d -> ((toRange xScale . (fromIntegral :: Int -> Double) . mdId) d, (toRange yScale . mdBid) d)) dataSeries
scaledAsks = map (\d -> ((toRange xScale . (fromIntegral :: Int -> Double) . mdId) d, (toRange yScale . mdAsk) d)) dataSeries

-- Converting the scaled data series to a Point with x and y coordinates
--  corresponding to the w and h attributes of this chart.
--  The above scaledDataSeries converts to [P (V2 0.0 0.0),P (V2 10.0 12.5),P (V2 20.0 25.0),P (V2 30.0 37.5),P (V2 40.0 62.5),P (V2 50.0 100.0),P (V2 60.0 25.0),P (V2 70.0 37.5),P (V2 80.0 50.0),P (V2 90.0 75.0),P (V2 100.0 12.5)]

pointsOfScaledBids :: [P2 Double]
pointsOfScaledBids = map (uncurry scalify) scaledBids

pointsOfScaledAsks :: [P2 Double]
pointsOfScaledAsks = map (uncurry scalify) scaledAsks

-- Scale any fraction to an absolute coordinate.

scalify :: Double -> Double -> P2 Double
scalify x y = p2 (x * chartWidth, y * chartHeight)

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
dot = (showOrigin . fc blue . circle) 2

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.

frame :: QDiagram B V2 Double Any
frame = lineWidth ultraThin (rect frameWidth frameHeight)

xAxis, yAxis :: QDiagram B V2 Double Any
xAxis = fromVertices [p2(0,0),p2(chartWidth,0)]
yAxis = fromVertices [p2(0,0),p2(0,chartHeight)]

-- Overlay the dot on the above frame.

-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.

chart :: QDiagram B V2 Double Any
chart = atop (centerXY
                     (xAxis
                       <> yAxis
                       <> (position
                             (zip pointsOfScaledBids (repeat dot)))
                       <> (position
                             (zip pointsOfScaledAsks (repeat dot)))
                       <> (fromVertices pointsOfScaledBids)
                       <> (fromVertices pointsOfScaledAsks)))
             (centerXY frame)

-- chart = mconcat [frame,dot]
-- chart = frame ||| dot
-- chart = juxtapose unitX dot frame
-- chart = dot <> frame

-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.

margin, frameWidth, frameHeight :: Double
frameWidth  = 100 + (2 * margin)
frameHeight = 100 + (2 * margin)
margin = 20

chartWidth,chartHeight :: Double
chartWidth  = 100
chartHeight = 100

-- Compile using similar commands as in Step 1.

-- The mainWith translates the logical units used in w and h to the
--  --width and --height arguments (actual units) provided at the
--  command line.

main :: IO ()
main = mainWith chart
