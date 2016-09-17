{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraphOpenGL
  (priceGraph)
  where

import Data.Colour.Names
import Data.Monoid
import "gl" Graphics.GL
import Linear.V2
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
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
priceGraph xScale yScale dataSeries = Picture scaledPrices GL_TRIANGLE_STRIP red Nothing
  where scaledPrices = (VS.concatMap v2ToVertex . VU.convert . VU.concatMap (scaledPrice xScale yScale) . VU.indexed) dataSeries

-- Scale from the domain (input data range) to the range (absolute coordinate).
-- TODO use a V3?
scaledPrice
  :: (Scale x
     ,Scale y)
  => x -> y -> (Int, PriceData) -> VU.Vector (V2 Double)
scaledPrice xScale yScale (x,d) =
    VU.cons (V2 ((toRange xScale . fromIntegral) x) ((toRange yScale) b))
     (VU.singleton (V2 ((toRange xScale . fromIntegral) x) ((toRange yScale) a)))
  where b = bid d
        a = ask d
