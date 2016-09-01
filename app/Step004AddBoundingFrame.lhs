> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}

> module Main where
>
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude

Draw a single blue coloured dot at x = 20 and y = 10. Also, show the
  local origin.

Related conversation on #diagrams:
 What am I doing wrong here? translate (2 ^& 3) == translateX 2 . translateY 3, correct?
 The circle is drawn but is not moving away from the edge
 <byorgey> joe9: coordinates in diagrams are always relative, and it recenters and resizes the diagram so it fits exactly in the output image
 <byorgey> joe9: so  circle 1 # translate (2 ^& 3)  does indeed move the circle to the point (2,3), but then when it renders it, it centers the output on (2,3) since that's where the circle is
 <byorgey> usually this is a good thing because it frees you from having to think about where things are, or how big they are
 <byorgey> but I can see how it can be confusing if you want to make a chart.
 <byorgey> I suggest making a "canvas" first by making a large rectangle of the size you want your background to be, then drawing stuff on top of that.  You can even make it aninvisible rectangle.

> dot :: Diagram B
> dot = (translateY 10 . translateX 5 . showOrigin . fc blue . circle) 1

Add a frame for the chart. The frame dimensions are the width and
  height provided on the command line.

> frame :: QDiagram B V2 Double Any
> frame = rect w h

The size of the chart, in logical units. All the diagrams use the
  logical units. The translation from the actual units to the logical
  units is done by the renderer. 100 corresponds to 100%.

> w,h :: Double
> w = 100
> h = 100

Compile using similar commands as in Step 1. The mainWith translates
  the logical units used in w and h to the --width and --height
  arguments (actual units) provided at the command line.

> main :: IO ()
> main = mainWith Main.frame
