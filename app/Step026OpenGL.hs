{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Prelude hiding (init)
-- import           System.Exit (exitFailure)
-- import Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
import           Control.Monad            (unless, when)
import           Data.Colour.SRGB
import qualified Data.IntMap.Strict       as IntMap
import           Data.IORef
import           Data.Maybe               (isNothing)
import           Data.Maybe
import qualified Data.Vector.Storable     as V
import           "gl" Graphics.GL
import           Graphics.UI.GLFW         as GLFW
--
import BulkVerticesData
import ChartOpenGL
import GLFWStuff
import MyData
import OpenGLStuff
import TypesOpenGL

-- | Geometry data is a list of four 2D vertices.
vertexBufferData :: V.Vector GLfloat
vertexBufferData = V.fromList [-1,-1,1,-1,-1,1,1,1]

vertices :: V.Vector GLfloat
-- vertices = V.fromList [-0.6,-0.4,0.6,-0.4,0,0.6]
vertices = V.fromList [-0.9,-0.9,-0.9,0.9,0.9,0.9,0.9,-0.9]

-- main :: IO ()
-- main = withGLFW $ window drawWindow
-- --   withGLFW $
-- --   do a <- asyncBound window
-- --      b <- asyncBound window
-- --      wait a
-- --      putStrLn "first window closed"
-- --      wait b
-- --      putStrLn "second window closed"
-- --      threadDelay (1 * 1000 * 1000)
main :: IO ()
main =
  do dataSeries <- buildDataSeries
     -- dataSeries <- return staticDataSeries
     ref <- newIORef dataSeries
     a <- async (threadDelay (1 * 1000 * 1000) >> updatedData ref dataSeries)
     (withGLFW . window) (renderChart ref)
     cancel a

drawWindow
  :: Window -> ColorUniformLocation -> State -> IO ()
drawWindow window colorUniformLocation _ =
  justDraw window colorUniformLocation vertices GL_TRIANGLES 1 0 0 1

--      justDraw window colorUniformLocation
--        vertices GL_LINE_LOOP 1 0 0 1
renderChart :: IORef (IntMap.IntMap MyData)
            -> Window
            -> ColorUniformLocation
            -> State
            -> IO ()
renderChart ref window colorUniformLocation state =
  do series <- readIORef ref
     -- then draw the diagram
     -- With OpenGL, the coordinates should be in the range (-1, 1)
     drawPictures
       window
       colorUniformLocation
       (chart -- 2  -- (fromIntegral (stateWindowWidth state))
              -- 2 -- (fromIntegral (stateWindowHeight state))
              (dataSeriesList series))

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
--      updatedData ref series

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
