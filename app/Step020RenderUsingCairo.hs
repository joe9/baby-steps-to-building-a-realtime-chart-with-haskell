{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude                hiding (dot, frame, set)
import Graphics.Rendering.Cairo        hiding (scale)
import Graphics.UI.Gtk                 hiding (Scale, rectangle)
--
import Axis
import PriceGraph
import Scale
import VolumeGraph

data MyData =
  MyData {mdId     :: Int
         ,mdBid    :: Double
         ,mdAsk    :: Double
         ,mdVolume :: Double}
  deriving (Eq,Read,Show)

dataSeries :: [MyData]
dataSeries =
  [MyData 1 1.19 1.26 1000
  ,MyData 2 1.22 1.27 2000
  ,MyData 3 1.27 1.37 1000
  ,MyData 4 1.37 1.47 0
  ,MyData 5 1.67 1.97 3000
  ,MyData 6 1.57 1.67 1000
  ,MyData 7 1.47 1.57 1000
  ,MyData 8 1.27 1.37 500
  ,MyData 9 1.17 1.25 5000
  ,MyData 10 1.1 1.15 0]

xScale, priceScale, volumeScale :: LinearScale
xScale =
  LinearScale (map (fromIntegral . mdId) dataSeries)
              0
              chartWidth

priceScale =
  LinearScale (concatMap (\d -> [mdBid d,mdAsk d]) dataSeries)
              0
              priceChartHeight

volumeScale =
  LinearScale (map mdVolume dataSeries)
              0
              volumeChartHeight

-- Add a frame for the chart. The frame dimensions are the width and
--  height provided on the command line.
frame :: QDiagram B V2 Double Any
frame = (showOrigin . lineWidth ultraThin . rect frameWidth) frameHeight

pChart :: QDiagram B V2 Double Any
pChart =
  --   showEnvelope
  (priceGraph xScale
              priceScale
              (map (\d -> (mdId d,mdBid d)) dataSeries)
              (map (\d -> (mdId d,mdAsk d)) dataSeries))

vChart :: QDiagram B V2 Double Any
vChart =
  --   showEnvelope
  (volumeGraph xScale
               volumeScale
               (map (\d -> (mdId d,mdVolume d)) dataSeries))

chart :: QDiagram B V2 Double Any
chart =
  position [(p2 (frameWidth / 2,frameHeight / 2),frame)
           ,(p2 (margin,margin + volumeChartHeight),pChart)
           ,(p2 (margin,margin),vChart)
           ,(p2 (frameWidth / 2,margin),bottomAxis xScale)
           ,(p2 (frameWidth / 2,margin + volumeChartHeight),bottomAxis xScale)
           ,(p2 (frameWidth / 2,frameHeight - margin),topAxis xScale)
           ,(p2 (margin,margin + volumeChartHeight + (priceChartHeight / 2))
            ,leftAxis priceScale)
           ,(p2 (margin,margin + (volumeChartHeight / 2)),leftAxis volumeScale)
           ,(p2 (frameWidth - margin
                ,margin + volumeChartHeight + (priceChartHeight / 2))
            ,rightAxis priceScale)
           ,(p2 (frameWidth - margin,margin + (volumeChartHeight / 2))
            ,rightAxis volumeScale)]

-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.
margin, frameWidth, frameHeight :: Double
frameWidth = 1000 + (2 * margin)

frameHeight = 1000 + (2 * margin)

margin = 20

chartWidth, chartHeight, priceChartHeight, volumeChartHeight
  :: Double
chartWidth = 1000

chartHeight = 1000

priceChartHeight = 800

volumeChartHeight = 200

renderDiagram
  :: Int -> Int -> Diagram Cairo -> Graphics.Rendering.Cairo.Render ()
renderDiagram w h c =
  snd (renderDia Cairo
                 ((CairoOptions
                     ""
                     (mkSizeSpec2D (Just (fromIntegral w))
                                   (Just (fromIntegral h)))
                     RenderOnly
                     False) :: Options Cairo V2 Double)
                 c)

main :: IO ()
main =
  do _ <- initGUI
     window <- windowNew
     let rows = 30
         columns = 11
     table <- tableNew rows columns True
     set window [windowTitle := "Hello Cairo",containerChild := table]
     canvas <- drawingAreaNew
     tableAttachDefaults table
                         canvas
                         0
                         columns
                         0
                         (rows - 3)
     widgetModifyBg canvas
                    StateNormal
                    (Color 65535 65535 65535)

     label <- labelNew (Just ( "initializing" :: String))
     tableAttachDefaults table label 0 0 (rows - 3) (rows - 2)

     widgetShowAll window
     _ <-
       onExpose canvas
                (\_ ->
                   do (w,h) <- widgetGetSize canvas
                      drawin <- widgetGetDrawWindow canvas
                      renderWithDrawable drawin
                                         (renderDiagram w h chart)
                      return True)
     _ <- onDestroy window mainQuit
     updateLabel label (show (last dataSeries))
     mainGUI

updateLabel :: Label -> String -> IO ()
updateLabel label title = do
  postGUISync (labelSetText label title)
