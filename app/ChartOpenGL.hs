{-# LANGUAGE PackageImports #-}

module ChartOpenGL where

import Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import "gl" Graphics.GL
--
import GLFWStuff
import OpenGLStuff
import ScaleDataUnboxedVector
import TypesOpenGL

-- import PriceGraphOpenGL
-- import VolumeGraphOpenGL
minimumElement, maximumElement
    :: (Ord a, VU.Unbox b)
    => (b -> a) -> VU.Vector b -> a
minimumElement f =
    f .
    VU.minimumBy
        (\a b ->
              compare (f a) (f b))

maximumElement f =
    f .
    VU.maximumBy
        (\a b ->
              compare (f a) (f b))

xScale, priceScale, volumeScale :: VU.Vector PriceData -> Scale
xScale dataSeries =
    linearScale
        0
        (fromIntegral (VU.length dataSeries - 1))
        (-1 + margin)
        (1 - margin)

priceScale dataSeries =
    linearScale
        (min (minimumElement bid dataSeries) (minimumElement ask dataSeries))
        (max (maximumElement bid dataSeries) (maximumElement ask dataSeries))
        (-1 + margin + volumeChartHeight 2)
        (-1 + margin + volumeChartHeight 2 + priceChartHeight 2)

volumeScale dataSeries =
    linearScale
        (minimumElement volume dataSeries)
        (maximumElement volume dataSeries)
        (-1 + margin)
        (-1 + margin + volumeChartHeight 2)

drawFrameFunction
    :: State
    -> VU.Vector PriceData
    -> Scale
    -> Scale
    -> Scale
    -> Drawable
    -> IO (IO ())
drawFrameFunction _ _ _ _ _ d = do
    let vertices =
            VS.fromList [-0.99, -0.99, -0.99, 0.99, 0.99, 0.99, 0.99, -0.99]
    loadBuffer (dBufferId d) vertices
    putStrLn ("frame vertices are: " ++ show vertices)
    return
        (glDrawArrays
             GL_LINE_LOOP
             0
             (div (fromIntegral (VS.length vertices)) 2))

-- Add a frame for the chart.
frameDrawable
    :: VertexArrayId -> BufferId -> Drawable
frameDrawable vaId bId =
    Drawable
    { dDraw = return ()
    , dLoadBufferAndBuildDrawFunction = drawFrameFunction
    , dPreviousValue = Nothing
    , dCurrentValue = (\s _ ->
                            ValueCursorPosition
                                (stateCursorX s)
                                (stateCursorY s))
    , dVertexArrayId = vaId
    , dBufferId = bId
    , dColour = green
    , dTransparency = Nothing
    , dType = Frame
    }

-- chart :: (Scale xscale
--          ,Scale priceScale
--          ,Scale volumeScale)
--       => xscale
--       -> priceScale
--       -> volumeScale
--       -> VU.Vector PriceData
--       -> [Picture]
-- chart x p v dataSeries =
--   [ -- frame
-- --    pChart x p dataSeries
--    vChart x v dataSeries
--   ,horizontalCrosshair 0.5
--   ,verticalCrosshair 0.25]
-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.
margin
    :: Double
margin = 0.05

chartWidth, chartHeight, priceChartHeight, volumeChartHeight :: Double -> Double
chartWidth w = w - (2 * margin)

chartHeight h = h - (2 * margin)

priceChartHeight = (* 0.8) . chartHeight

volumeChartHeight = (* 0.2) . chartHeight

-- horizontalCrosshair :: Double -> Picture
-- horizontalCrosshair y =
--   Picture (VS.fromList [-1,realToFrac y,1,realToFrac y])
--           GL_LINES
--           green
--           (Just 0.5)
-- verticalCrosshair :: Double -> Picture
-- verticalCrosshair x =
--   Picture (VS.fromList [realToFrac x,-1,realToFrac x,1])
--           GL_LINES
--           green
--           (Just 0.5)
-- (0,0) for the cursor position is the top left corner
buildHorizontalCrosshair
    :: State
    -> VU.Vector PriceData
    -> Scale
    -> Scale
    -> Scale
    -> Drawable
    -> IO (IO ())
buildHorizontalCrosshair state _ _ _ _ d
  | 0 > stateCursorY state || 0 == stateWindowHeight state = return (return ())
  | otherwise = do
      let f = fromIntegral :: Int -> Double
          ny = fromIntegral (stateWindowHeight state) - stateCursorY state
          y = (2 * ny / f (stateWindowHeight state)) - 1
          vertices = VS.fromList [-1, realToFrac y, 1, realToFrac y]
      loadBuffer (dBufferId d) vertices
      return
          (glDrawArrays GL_LINES 0 (div (fromIntegral (VS.length vertices)) 2))

horizontalCrosshairDrawable :: VertexArrayId -> BufferId -> Drawable
horizontalCrosshairDrawable vaId bId =
    Drawable
    { dDraw = return ()
    , dLoadBufferAndBuildDrawFunction = buildHorizontalCrosshair
    , dPreviousValue = Nothing
    , dCurrentValue = (\s _ ->
                            (ValueCursorPosition
                                 (stateCursorX s)
                                 (stateCursorY s)))
    , dVertexArrayId = vaId
    , dBufferId = bId
    , dColour = green
    , dTransparency = Just 0.5
    , dType = HorizontalCrosshair
    }

buildVerticalCrosshair
    :: State
    -> VU.Vector PriceData
    -> Scale
    -> Scale
    -> Scale
    -> Drawable
    -> IO (IO ())
-- (0,0) for the cursor position is the top left corner
buildVerticalCrosshair state _ _ _ _ d
  | 0 > stateCursorX state || 0 == stateWindowWidth state = return (return ())
  | otherwise = do
      let f = fromIntegral :: Int -> Double
          x = ((2 * stateCursorX state) / f (stateWindowWidth state)) - 1
          vertices = VS.fromList [realToFrac x, -1, realToFrac x, 1]
      loadBuffer (dBufferId d) vertices
      return
          (glDrawArrays GL_LINES 0 (div (fromIntegral (VS.length vertices)) 2))

verticalCrosshairDrawable :: VertexArrayId -> BufferId -> Drawable
verticalCrosshairDrawable vaId bId =
    Drawable
    { dDraw = return ()
    , dLoadBufferAndBuildDrawFunction = buildVerticalCrosshair
    , dPreviousValue = Nothing
    , dCurrentValue = \s _ ->
                           (ValueCursorPosition
                                (stateCursorX s)
                                (stateCursorY s))
    , dVertexArrayId = vaId
    , dBufferId = bId
    , dColour = green
    , dTransparency = Just 0.5
    , dType = VerticalCrosshair
    }
