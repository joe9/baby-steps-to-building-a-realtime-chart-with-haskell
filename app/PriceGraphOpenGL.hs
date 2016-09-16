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
--
import OpenGLStuff
import ScaleV2
import TypesOpenGL

type Price = (Int,Bid,Ask)

-- TODO dots
--   Pictures ([areaBetweenBidAndAsk areaVertices] <> map dot scaledBids <>
--             map dot scaledAsks)
priceGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Price] -> Picture
priceGraph xScale yScale prices = Picture scaledPrices GL_TRIANGLE_STRIP red Nothing
  where scaledPrices = concatMap (scaledPrice xScale yScale) prices

-- Scale from the domain (input data range) to the range (absolute coordinate).
-- TODO use a V3?
scaledPrice
  :: (Scale x
     ,Scale y)
  => x -> y -> (Int,Bid,Ask) -> [(V2 Double)]
scaledPrice xScale yScale (i,b,a) =
  [V2 ((toRange xScale . fromIntegral) i)
      ((toRange yScale) b)
  ,(V2 ((toRange xScale . fromIntegral) i)
       ((toRange yScale) a))]
