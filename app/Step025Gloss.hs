{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           System.Random
import           Data.List
import           Control.Concurrent.Async
import qualified Data.IntMap.Strict                  as IntMap
import           Data.IORef
import           Graphics.Gloss.Interface.IO.Animate
--
import ChartGloss
import TypesGloss
import MyData

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
main = do
  dataSeries <- buildDataSeries
  ref <- newIORef dataSeries
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

dataSeriesList
  :: IntMap.IntMap MyData -> [(Int,Bid,Ask,Volume)]
dataSeriesList =
  fmap (\(_,d) -> (mdId d,mdBid d,mdAsk d,mdVolume d)) . IntMap.toList
