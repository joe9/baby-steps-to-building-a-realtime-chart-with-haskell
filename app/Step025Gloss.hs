{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Data.IntMap.Strict                  as IntMap
import           Data.IORef
import           Graphics.Gloss.Interface.IO.Animate
--
import ChartGloss
import TypesGloss

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
-- http://stackoverflow.com/questions/26848694/cairo-flips-a-drawing
renderChart :: IORef (IntMap.IntMap MyData)
            -> Width
            -> Height
            -> Float -- time
            -> IO Picture
renderChart ref w h _ =
  do series <- readIORef ref
     -- then draw the diagram
     return
       (Pictures [chart w h (dataSeriesList series)
                 ,(translate (-100)
                             (-100) .
                   scale 0.1 0.1 . text . show . IntMap.findMax) series])

main :: IO ()
main =
  do ref <- newIORef dataSeries
     a <- async (threadDelay (1 * 1000 * 1000) >> updatedData ref dataSeries)
     let w = 100
         h = 100
     animateIO (InWindow "Realtime Chart"
                         (w,h)
                         (0,0))
               white
               (renderChart ref w h)
               controllerSetRedraw
     cancel a

-- updateLabel :: Label -> String -> IO ()
-- updateLabel label title = labelSetText label title
dataSeriesList
  :: IntMap.IntMap MyData -> [(Int,Bid,Ask,Volume)]
dataSeriesList =
  fmap (\(_,d) -> (mdId d,mdBid d,mdAsk d,mdVolume d)) . IntMap.toList

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData
  :: IORef (IntMap.IntMap MyData) -> IntMap.IntMap MyData -> IO b
updatedData ref series =
  do let newSeries = addAnother series
     atomicModifyIORef' ref
                        (\_ -> (newSeries,()))
     threadDelay (1 * 1000 * 1000)
     updatedData ref newSeries

addAnother
  :: IntMap.IntMap MyData -> IntMap.IntMap MyData
addAnother ds =
  IntMap.insert
    (1 + mdId d)
    (MyData (1 + mdId d)
            (1 + mdBid d)
            (1 + mdAsk d)
            (1 + mdVolume d))
    ds
  where (_,d) = IntMap.findMax ds
