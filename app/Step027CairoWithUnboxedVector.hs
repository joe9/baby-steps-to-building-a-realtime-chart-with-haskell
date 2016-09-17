{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Data.Vector.Unboxed as VU
import           Data.IORef
import           Graphics.Rendering.Cairo        hiding (scale)
import           Graphics.Rendering.Cairo.Matrix hiding (translate)
import           Graphics.UI.Gtk                 hiding (Scale,
                                                  rectangle)
--
import ChartCairoWithUnboxedVector
import TypesOpenGL
import MyDataUnboxedVector

-- http://code.haskell.org/gtk2hs/docs/tutorial/Tutorial_Port/app1.xhtml
-- http://stackoverflow.com/questions/26848694/cairo-flips-a-drawing
renderChart :: (Width -> Height -> VU.Vector PriceData -> Render ())
            -> Width
            -> Height
            -> VU.Vector PriceData
            -> Graphics.Rendering.Cairo.Render ()
renderChart f w h ds = do
--   m <- getMatrix
--   liftIO ( print m )
  -- cairo sets the origin (0,0) in the top left corner
  -- it is easier when the origin (0,0) is in the bottom left corner
--   transform (Matrix 1 0 0 (-1) 0 0) >> translate 0 (-h)
  -- below line is the same as the above
  setMatrix (Matrix 1 0 0 (-1) 0 h)
--   n <- getMatrix
--   liftIO ( print n )
  -- clear the drawing window
  setSourceRGB 1 1 1
  paint
  -- then draw the diagram
  f w h ds

main :: IO ()
main =
  do dataSeries <- buildDataSeries
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
                       renderChart chart
                                   (fromIntegral w)
                                   (fromIntegral h)
                       ) series
                      return False)
     _ <- onDestroy window mainQuit
     a <-
       async (threadDelay (1 * 1000 * 1000) >>
              updatedData ref canvas label dataSeries)
     mainGUI
     cancel a

updateLabel :: Label -> String -> IO ()
updateLabel label title = labelSetText label title

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData :: WidgetClass widget
            => IORef (VU.Vector PriceData)
            -> widget
            -> Label
            -> VU.Vector PriceData
            -> IO b
updatedData ref canvas label series =
  do let newSeries = addAnother series
     atomicModifyIORef' ref
                        (\_ -> (newSeries,()))
     postGUIAsync
       ((updateLabel label . show . VU.last) newSeries >>
        widgetQueueDraw canvas)
     threadDelay (1 * 1000 * 1000)
     updatedData ref canvas label newSeries

addAnother :: VU.Vector PriceData -> VU.Vector PriceData
addAnother ds =
  VU.snoc ds (b,a,v)
  where d = VU.last ds
        b = 1 + bid d
        a = 1 + ask d
        v = 1000 + volume d
