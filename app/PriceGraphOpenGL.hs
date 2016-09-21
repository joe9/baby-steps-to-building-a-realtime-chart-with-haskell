{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module PriceGraphOpenGL
  (priceChartDrawable)
  where

import Data.Colour.Names
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import "gl" Graphics.GL
-- import           Linear.V2
--
import OpenGLStuff
import ScaleDataUnboxedVector
import TypesOpenGL

priceChartDrawable :: VertexArrayId -> BufferId -> Drawable
priceChartDrawable vaId bId =
    Drawable
    { dDraw = return ()
    , dLoadBufferAndBuildDrawFunction = \_ dataSeries scalex scaleprice _ d -> do
          let vertices = priceBufferData scalex scaleprice dataSeries
          loadBuffer (dBufferId d) vertices
          return
              (glDrawArrays
                   GL_TRIANGLE_STRIP
                   0
                   (div (fromIntegral (VS.length vertices)) 2))
    , dPreviousValue = Nothing
    , dCurrentValue = \_ ->
                           ValueAsOf . asof . VU.last
    , dVertexArrayId = vaId
    , dBufferId = bId
    , dColour = lightpink
    , dTransparency = Nothing
    , dType = PriceChart
    }

-- TODO dots
--   Pictures ([areaBetweenBidAndAsk areaVertices] <> map dot scaledBids <>
--             map dot scaledAsks)
priceGraph
    :: Scale -> Scale -> VU.Vector PriceData -> Picture
priceGraph scalex scaley dataSeries =
    Picture scaledPrices GL_TRIANGLE_STRIP lightpink Nothing
  where
    scaledPrices =
        (VU.convert . VU.concatMap (scaledPrice scalex scaley) . VU.indexed)
            dataSeries

priceBufferData :: Scale -> Scale -> VU.Vector PriceData -> VS.Vector Float
priceBufferData scalex scaley =
    VU.convert . VU.concatMap (scaledPrice scalex scaley) . VU.indexed

-- scaledPriceOld
--   :: (Scale x
--      ,Scale y)
--   => x -> y -> (Int,PriceData) -> VU.Vector (V2 Double)
-- scaledPriceOld xScale yScale (x,d) =
--   VU.cons (V2 ((toRange xScale . fromIntegral) x)
--               ((toRange yScale) b))
--           (VU.singleton
--              (V2 ((toRange xScale . fromIntegral) x)
--                  ((toRange yScale) a)))
--   where b = bid d
--         a = ask d
--   where scaledPrices = (VS.concatMap v2ToVertex . VU.convert . VU.concatMap (scaledPriceOld xScale yScale) . VU.indexed) dataSeries
-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledPrice
    :: Scale -> Scale -> (Int, PriceData) -> VU.Vector Float
scaledPrice scalex scaley (x,d) =
    VU.fromList
        [ (realToFrac . sToRange scalex scalex . fromIntegral) x
        , (realToFrac . (sToRange scaley) scaley) b
        , (realToFrac . sToRange scalex scalex . fromIntegral) x
        , (realToFrac . (sToRange scaley) scaley) a]
  where
    b = bid d
    a = ask d
