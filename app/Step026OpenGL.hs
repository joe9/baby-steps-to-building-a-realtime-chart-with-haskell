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
import           Data.IORef
import           Data.Maybe               (isNothing)
import           Data.Maybe
import qualified Data.Vector.Storable     as V
import qualified Data.Vector.Unboxed      as VU
import           "gl" Graphics.GL
import           Graphics.UI.GLFW         as GLFW
import           System.Exit
import           System.Random
--
import BulkVerticesData
import ChartOpenGL
import GLFWStuff
import MyDataUnboxedVector
import OpenGLStuff
import ScaleUnboxedVector
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
     let xscale = xScale dataSeries
         pricescale = priceScale dataSeries
         volumescale = volumeScale dataSeries
     putStrLn ("dataSeries: " ++ show dataSeries)
     putStrLn ("xScale: " ++ show xscale)
     putStrLn ("pricescale: " ++ show pricescale)
     putStrLn ("volumeScale: " ++ show volumescale)
     ref <- newIORef (dataSeries,xscale,pricescale,volumescale)
     a <-
       async (threadDelay (1 * 1000 * 1000) >>
              updatedData ref
                          (dataSeries,xscale,pricescale,volumescale))
     (withGLFW . window) (renderChart ref)
     cancel a

drawWindow
  :: Window -> ColorUniformLocation -> State -> IO ()
drawWindow window colorUniformLocation _ =
  justDraw window colorUniformLocation vertices GL_TRIANGLES 1 0 0 1

--      justDraw window colorUniformLocation
--        vertices GL_LINE_LOOP 1 0 0 1
renderChart :: IORef (VU.Vector PriceData,LinearScale,LinearScale,LinearScale)
            -> Window
            -> ColorUniformLocation
            -> State
            -> IO ()
renderChart ref window colorUniformLocation state =
  do (series,xscale,pricescale,volumescale) <- readIORef ref
     -- then draw the diagram
     -- With OpenGL, the coordinates should be in the range (-1, 1)
     drawPictures window
                  colorUniformLocation
                  (chart xscale pricescale volumescale series)

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData :: IORef (VU.Vector PriceData,LinearScale,LinearScale,LinearScale)
            -> (VU.Vector PriceData,LinearScale,LinearScale,LinearScale)
            -> IO b
updatedData ref oldData =
  do newSeries <- addAnother oldData
     atomicModifyIORef' ref
                        (\_ -> (newSeries,()))
     threadDelay (1 * 1000 * 1000)
     updatedData ref newSeries

addAnother
  :: (VU.Vector PriceData,LinearScale,LinearScale,LinearScale)
  -> IO (VU.Vector PriceData,LinearScale,LinearScale,LinearScale)
addAnother (series,xscale,pricescale,volumescale) =
  do b <- randomRIO (1,2)
     a <- randomRIO (2,3)
     v <- randomRIO (0,1000000)
     return (VU.snoc series (b,a,v)
            ,addToDomain xscale
                         (fromIntegral (VU.length series))
            ,addToDomain (addToDomain pricescale b)
                         a
            ,addToDomain volumescale v)
  where d = VU.last series
