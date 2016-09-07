{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Graphics.Rendering.Cairo        hiding (scale)
import Graphics.UI.Gtk                 hiding (Scale, rectangle)
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
--
import ChartCairo
import Types

data MyData =
  MyData {mdId     :: Int
         ,mdBid    :: Double
         ,mdAsk    :: Double
         ,mdVolume :: Double}
  deriving (Eq,Read,Show)

dataSeries :: IntMap.IntMap MyData
dataSeries =
  (IntMap.fromList . map (\d -> (mdId d,d)))
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

-- http://code.haskell.org/gtk2hs/docs/tutorial/Tutorial_Port/app1.xhtml
renderDiagram
  :: Render () -> Graphics.Rendering.Cairo.Render ()
renderDiagram f =
  -- clear the drawing window
  setSourceRGB 1 1 1 >> paint >>
  -- then draw the diagram
  f

main :: IO ()
main =
  do
     ref <- newIORef dataSeries
     _ <- initGUI
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
                      series <- readIORef ref
                      (renderWithDrawable drawin .
                       renderDiagram . chart (fromIntegral w) (fromIntegral h) . dataSeriesList) series
                      return False)
     _ <- onDestroy window mainQuit
     a <-
       async (threadDelay (10 * 1000 * 1000) >>
              updatedData ref canvas label dataSeries)
     mainGUI
     cancel a

updateLabel :: Label -> String -> IO ()
updateLabel label title = labelSetText label title

dataSeriesList
  :: IntMap.IntMap MyData -> [(Int,Bid,Ask,Volume)]
dataSeriesList = fmap (\(_,d) -> (mdId d,mdBid d,mdAsk d,mdVolume d)) . IntMap.toList

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData
  :: WidgetClass widget
  => IORef (IntMap.IntMap MyData) -> widget -> Label -> IntMap.IntMap MyData -> IO b
updatedData ref canvas label series =
  do
     let newSeries = addAnother series
     atomicModifyIORef' ref (\_ -> (newSeries,()))
     postGUIAsync
       ((updateLabel label . show . IntMap.findMax) newSeries >> widgetQueueDraw canvas)
     threadDelay (1 * 1000 * 1000)
     updatedData ref canvas
                 label
                 newSeries

addAnother :: IntMap.IntMap MyData -> IntMap.IntMap MyData
addAnother ds =
    IntMap.insert (1 + mdId d)
           (MyData (1 + mdId d) (1 + mdBid d) (1 + mdAsk d) (1 + mdVolume d))
           ds
    where (_,d) = IntMap.findMax ds
