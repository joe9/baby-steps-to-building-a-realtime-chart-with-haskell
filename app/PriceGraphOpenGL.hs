{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraphOpenGL
  (priceGraph)
  where

import           Data.Colour.Names
import qualified Data.Vector.Unboxed  as VU
import           "gl" Graphics.GL
import           Linear.V2
-- import qualified Data.Vector.Storable as VS
--
import OpenGLStuff
import ScaleUnboxedVector
import TypesOpenGL

-- TODO dots
--   Pictures ([areaBetweenBidAndAsk areaVertices] <> map dot scaledBids <>
--             map dot scaledAsks)
priceGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> VU.Vector PriceData -> Picture
priceGraph xScale yScale dataSeries =
  Picture scaledPrices GL_TRIANGLE_STRIP red Nothing
  where scaledPrices =
          (VU.convert . VU.concatMap (scaledPrice xScale yScale) . VU.indexed) dataSeries
--   where scaledPrices = (VS.concatMap v2ToVertex . VU.convert . VU.concatMap (scaledPriceOld xScale yScale) . VU.indexed) dataSeries
-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledPrice
  :: (Scale x
     ,Scale y)
  => x -> y -> (Int,PriceData) -> VU.Vector Float
scaledPrice xScale yScale (x,d) =
  VU.fromList
    [(realToFrac . toRange xScale . fromIntegral) x
    ,(realToFrac . toRange yScale) b
    ,(realToFrac . toRange xScale . fromIntegral) x
    ,(realToFrac . toRange yScale) a]
  where b = bid d
        a = ask d

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
