{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude                hiding (dot, frame, set)
import Graphics.Rendering.Cairo        hiding (scale)
import Graphics.UI.Gtk                 hiding (Scale, rectangle)
--
import Chart

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
     label <- labelNew (Just ("initializing" :: String))
     tableAttachDefaults table
                         label
                         0
                         columns
                         (rows - 3)
                         (rows - 2)
     widgetShowAll window
     _ <-
       onExpose canvas
                (\_ ->
                   do (w,h) <- widgetGetSize canvas
                      drawin <- widgetGetDrawWindow canvas
                      (renderWithDrawable drawin .
                       renderDiagram w h . chart . dataSeriesList) dataSeries
                      return False)
     _ <- onDestroy window mainQuit
--      (updateLabel label . show . last) dataSeries
--      updateChart canvas dataSeries
     a <-
       async (threadDelay (10 * 1000 * 1000) >>
              updatedData canvas label dataSeries)
     mainGUI
     cancel a

updateLabel :: Label -> String -> IO ()
updateLabel label title = labelSetText label title

dataSeriesList
  :: [MyData] -> [(Int,Bid,Ask,Volume)]
dataSeriesList = map (\d -> (mdId d,mdBid d,mdAsk d,mdVolume d))

updatedData
  :: WidgetClass widget
  => widget -> Label -> [MyData] -> IO b
updatedData canvas label series =
  do postGUISync
       ((updateLabel label . show . last) series >> updateChart canvas series)
     threadDelay (1 * 1000 * 1000)
     updatedData canvas
                 label
                 (addAnother series)

addAnother :: [MyData] -> [MyData]
addAnother ds =
  ds ++
  [MyData (1 + mdId d)
          (1 + mdBid d)
          (1 + mdAsk d)
          (1 + mdVolume d)]
  where d = last ds

--     widgetModifyBg canvas
--                        StateNormal
--                        (Color 65535 65535 65535)
-- (renderWithDrawable drawin . renderDiagram w h . chart . dataSeriesList) series
updateChart :: WidgetClass widget
            => widget -> [MyData] -> IO ()
updateChart canvas series =
  do
     widgetModifyBg canvas
                    StateNormal
                    (Color 65535 65535 65535)
     widgetQueueDraw canvas
     _ <-
       onExpose canvas
                (\_ ->
                   do (w,h) <- widgetGetSize canvas
                      drawin <- widgetGetDrawWindow canvas
                      (renderWithDrawable drawin .
                       renderDiagram w h . chart . dataSeriesList) series
                      return False)
     widgetQueueDraw canvas
