{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Bits
import           Data.Colour.Names
import           Data.IORef
import           Data.Time.Clock.POSIX
import qualified Data.Vector.Storable     as VS
import qualified Data.Vector.Unboxed      as VU
import           "gl" Graphics.GL
import           Graphics.UI.GLFW         as GLFW
import           Prelude                  hiding (init)
import           System.Random
--
import ChartOpenGL
import PriceGraphOpenGL
import VolumeGraphOpenGL
import GLFWStuff
import MyDataUnboxedVector
import OpenGLStuff
import ScaleDataUnboxedVector
import TypesOpenGL

-- | Geometry data is a list of four 2D vertices.
vertexBufferData :: VS.Vector GLfloat
vertexBufferData = VS.fromList [-1,-1,1,-1,-1,1,1,1]

vertices :: VS.Vector GLfloat
-- vertices = VS.fromList [-0.6,-0.4,0.6,-0.4,0,0.6]
vertices = VS.fromList [-0.9,-0.9,-0.9,0.9,0.9,0.9,0.9,-0.9]

-- fonts can be added using freetype2 or FontyFruity. edwardk
-- recommends using Valve approach of rendering it with a signed
-- distance field
-- nanovg uses Modern OpenGL to render and is a next-generation
-- gloss. It is a good idea to check it when things get sticky.
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
--      putStrLn ("xScale: " ++ show xscale)
--      putStrLn ("pricescale: " ++ show pricescale)
--      putStrLn ("volumeScale: " ++ show volumescale)
     ref <- newIORef (dataSeries,xscale,pricescale,volumescale)
     a <-
       async (threadDelay (1 * 1000 * 1000) >>
              updatedData ref
                          (dataSeries,xscale,pricescale,volumescale))
     withGLFW (window onWindow (renderDrawables ref))
     cancel a

-- drawWindow
--   :: Window -> ColorUniformLocation -> State -> IO ()
-- drawWindow win colorUniformLocation _ =
--   justDraw win colorUniformLocation vertices GL_TRIANGLES 1 0 0 1
--      justDraw window colorUniformLocation
--        vertices GL_LINE_LOOP 1 0 0 1
onWindow :: ([Drawable] -> IO ()) -> IO ()
onWindow f = initializeDrawables f

renderDrawables :: IORef (VU.Vector PriceData
                         ,Scale
                         ,Scale
                         ,Scale)
                -> Window
                -> ColorUniformLocation
                -> State
                -> [Drawable]
                -> IO [Drawable]
renderDrawables ref win colorUniformLocation state ds = do
    putStrLn "renderDrawables called"
    (series,_,_,_) <- readIORef ref
    if (any (\d -> Just (currentValue d state series) /= previousValue d) ds)
      then mapM (renderDrawable ref win colorUniformLocation state) ds
      else return ds

renderDrawable :: IORef (VU.Vector PriceData
                         ,Scale
                         ,Scale
                         ,Scale)
                -> Window
                -> ColorUniformLocation
                -> State
                -> Drawable
                -> IO Drawable
renderDrawable ref win colorUniformLocation state drawable = do
  let justDraw = (\d -> do
                     drawWith win colorUniformLocation (vertexArrayId d) (bufferId d) (colour d) (transparency d) (draw d)
                     return d)
  (series,xscale,pricescale,volumescale) <- readIORef ref
  let newValue = currentValue drawable state series
  if (Just newValue /= previousValue drawable)
    then do
        putStrLn "renderDrawable called - loading buffer"
        -- With OpenGL, the coordinates should be in the range (-1, 1)
        drawFunction <- (loadBufferAndBuildDrawFunction drawable) state series xscale pricescale volumescale drawable
        justDraw (drawable {draw = drawFunction, previousValue = Just newValue})
    else justDraw drawable

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData :: IORef (VU.Vector PriceData,Scale,Scale,Scale)
            -> (VU.Vector PriceData,Scale,Scale,Scale)
            -> IO b
updatedData ref oldData =
  do newSeries <- addAnother oldData
     atomicModifyIORef' ref
                        (\_ -> (newSeries,()))
--      GLFW.postEmptyEvent
     threadDelay (1 * 1000 * 1000)
     updatedData ref newSeries

addAnother
  :: (VU.Vector PriceData,Scale,Scale,Scale)
  -> IO (VU.Vector PriceData,Scale,Scale,Scale)
addAnother (series,xscale,pricescale,volumescale) =
  do b <- randomRIO (1,2)
     a <- randomRIO (2,3)
     v <- randomRIO (0,1000000)
     asf <- fmap ((fromIntegral :: Integer -> AsOf) . round) getPOSIXTime
     return (VU.snoc series (b,a,v,asf)
            ,addToDomain xscale
                         (fromIntegral (VU.length series))
            ,addToDomain (addToDomain pricescale b)
                         a
            ,addToDomain volumescale v)
  where addToDomain s = ( sAddToDomain s) s

-- could use the ContT monad. but, this is more readable
--  https://github.com/glguy/irc-core/blob/v2/src/Client/CApi.hs#L146-L158
initializeDrawables
  :: ([Drawable] -> IO b) -> IO b
initializeDrawables continueFunction =
  withVertexArray $
  \svaid svabid ->
    do withVertexArray $
         \fvaid fvabid ->
           do withVertexArray $
                \pvaid pvabid ->
                  do withVertexArray $
                       \vvaid vvabid ->
                         do withVertexArray $
                              \hcvaid hcvabid ->
                                do withVertexArray $
                                     \vcvaid vcvabid ->
                                       do continueFunction
                                            [screenDrawable svaid svabid
                                            ,frameDrawable fvaid fvabid
                                            ,priceChartDrawable pvaid pvabid
                                            ,volumeChartDrawable vvaid vvabid
                                            ,horizontalCrosshairDrawable hcvaid hcvabid
                                            ,verticalCrosshairDrawable vcvaid vcvabid
                                            ]

screenDrawable
  :: VertexArrayId -> BufferId -> Drawable
screenDrawable vaId bId =
  let drawFunction =
              do glClearColor 0.05 0.05 0.05 1
                 glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  in Drawable {
            draw = drawFunction
           ,previousValue = Nothing
           ,currentValue = \_ _ -> ValueInt 0
           ,loadBufferAndBuildDrawFunction = (\_ _ _ _ _ _ -> return drawFunction)
           ,vertexArrayId = vaId
           ,bufferId = bId
           ,colour = red
           ,transparency = Nothing}
