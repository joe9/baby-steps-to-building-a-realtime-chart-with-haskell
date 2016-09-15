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
import           Data.Maybe               (isNothing)
import           Data.Maybe
import qualified Data.Vector.Storable     as V
import           "gl" Graphics.GL
import           Graphics.UI.GLFW         as GLFW
--
import GLFWStuff
import OpenGLStuff
import BulkVerticesData

-- | Geometry data is a list of four 2D vertices.
vertexBufferData :: V.Vector GLfloat
vertexBufferData = V.fromList [-1,-1,1,-1,-1,1,1,1]

vertices :: V.Vector GLfloat
vertices = V.fromList [-0.6,-0.4,0.6,-0.4,0,0.6]

-- https://www.opengl.org/wiki/Shader_Compile_Error
main :: IO ()
main = withGLFW $ window drawWindow
--   withGLFW $
--   do a <- asyncBound window
--      b <- asyncBound window
--      wait a
--      putStrLn "first window closed"
--      wait b
--      putStrLn "second window closed"
--      threadDelay (1 * 1000 * 1000)

startWindowOperations :: IO ()
startWindowOperations = withWindow 640 480 "sample drawWindow" drawWindow

drawWindow
  :: Window -> ColorUniformLocation -> IO ()
drawWindow window colorUniformLocation =
  do drawPictures window
                  colorUniformLocation
                  [Picture vertices GL_TRIANGLES (RGB 1 0 0,1)]
