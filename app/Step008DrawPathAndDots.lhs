> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}

> module Main where
>
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude hiding (dot,frame)

> dataSeries :: [(Double,Double)]
> dataSeries = [(0,10),(10,20),(20,30),(30,40),(40,60),(50,90),(60,30),(70,40),(80,50),(90,70),(100,20)]

Below from d3.js docs about domain and range.
  D for Domain, D for Data: Now that you know how to remember it,
  lets try to define it. Domain represents the boundaries within
  which your data lies. e.g. If I had an array of numbers with no
  number smaller than 1 and no number larger than 100, my domain
  would be 1 to 100.
  Range: There will not always be a direct mapping between your data
  points and an actual pixel on the screen. For example, if you are
  plotting a graph of sales figures and the sales is in tens of
  thousands, it is unlikely that you will be able to have a bar graph
  with the same pixel height as the data. In that case, you need to
  specify the boundaries within which your original data can be
  transformed. These boundaries are called the range.
  Scale: Now that you know what a domain and range is, you need a way to
  convert your data into corresponding values in the domain. And
  thats exactly what scales do.

  A scale's input domain is the range of possible input data
  values. Given the apples data above, appropriate input domains would
  be either 100 and 500 (the minimum and maximum values of the data
  set) or zero and 500.
  A scale's output range is the range of possible output values,
  commonly used as display values in pixel units. The output range is
  completely up to you, as the information designer. If you decide the
  shortest apple-bar will be 10 pixels tall, and the tallest will be
  350 pixels tall, then you could set an output range of 10 and 350.

> xDomainExtent, yDomainExtent :: (Double, Double)
> xDomainExtent =  ((minimum . map fst) dataSeries, (maximum . map fst) dataSeries)
> yDomainExtent =  ((minimum . map snd) dataSeries, (maximum . map snd) dataSeries)

scaledDataSeries is
 [(0.0,0.0),(0.1,0.125),(0.2,0.25),(0.3,0.375),(0.4,0.625),(0.5,1.0),(0.6,0.25),(0.7,0.375),(0.8,0.5),(0.9,0.75),(1.0,0.125)]

> scaledDataSeries :: [(Double,Double)]
> scaledDataSeries = map (\(x,y) -> ((x - minX)/(maxX - minX), (y - minY)/(maxY - minY))) dataSeries
>                    where minX = fst xDomainExtent
>                          maxX = snd xDomainExtent
>                          minY = fst yDomainExtent
>                          maxY = snd yDomainExtent

Converting the scaled data series to a Point with x and y coordinates
  corresponding to the w and h attributes of this chart.
  The above scaledDataSeries converts to [P (V2 0.0 0.0),P (V2 10.0 12.5),P (V2 20.0 25.0),P (V2 30.0 37.5),P (V2 40.0 62.5),P (V2 50.0 100.0),P (V2 60.0 25.0),P (V2 70.0 37.5),P (V2 80.0 50.0),P (V2 90.0 75.0),P (V2 100.0 12.5)]

> pointsOfScaledDataSeries :: [P2 Double]
> pointsOfScaledDataSeries = map (uncurry scalify) scaledDataSeries

Scale any fraction to an absolute coordinate.

> scalify :: Double -> Double -> P2 Double
> scalify x y = p2 (x * w, y * h)

Draw a single blue coloured dot showing the local origin.

Related conversation on #diagrams:
 What am I doing wrong here? translate (2 ^& 3) == translateX 2 . translateY 3, correct?
 The circle is drawn but is not moving away from the edge
 <byorgey> joe9: coordinates in diagrams are always relative, and it recenters and resizes the diagram so it fits exactly in the output image
 <byorgey> joe9: so  circle 1 # translate (2 ^& 3)  does indeed move the circle to the point (2,3), but then when it renders it, it centers the output on (2,3) since that's where the circle is
 <byorgey> usually this is a good thing because it frees you from having to think about where things are, or how big they are
 <byorgey> but I can see how it can be confusing if you want to make a chart.
 <byorgey> I suggest making a "canvas" first by making a large rectangle of the size you want your background to be, then drawing stuff on top of that.  You can even make it aninvisible rectangle.

> dot :: Diagram B
> dot = (showOrigin . fc blue . circle) 2

Add a frame for the chart. The frame dimensions are the width and
  height provided on the command line.

> frame :: QDiagram B V2 Double Any
> frame = rect w h

Overlay the dot on the above frame.

Do not assume that the frame will be positioned at the
  center. Explicitly position all the elements.

> chart :: QDiagram B V2 Double Any
> chart = atop (position (zip pointsOfScaledDataSeries (repeat dot) ++ [(scalify 0.5 0.5,frame)]))
>              (fromVertices pointsOfScaledDataSeries)

> -- chart = mconcat [frame,dot]
> -- chart = frame ||| dot
> -- chart = juxtapose unitX dot frame
> -- chart = dot <> frame

The size of the chart, in logical units. All the diagrams use the
  logical units. The translation from the actual units to the logical
  units is done by the renderer. 100 corresponds to 100%.

> w,h :: Double
> w = 100
> h = 100

Compile using similar commands as in Step 1.

The mainWith translates the logical units used in w and h to the
  --width and --height arguments (actual units) provided at the
  command line.

> main :: IO ()
> main = mainWith chart
