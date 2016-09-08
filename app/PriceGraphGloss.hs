{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraphGloss
  (priceGraph)
  where

import Data.Colour.Names
import Data.Monoid
import Graphics.Gloss
import Linear.V2
--
import ScaleV2
import TypesGloss hiding (Ask, Bid)

type Bid = (Int,Double)

type Ask = (Int,Double)

priceGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Bid] -> [Ask] -> Picture
priceGraph xScale yScale bids asks =
  Pictures ([areaBetweenBidAndAsk areaVertices] <> map dot scaledBids <>
            map dot scaledAsks)
  where scaledBids = scaledPoints xScale yScale bids
        scaledAsks = scaledPoints xScale yScale asks
        areaVertices = scaledBids ++ reverse scaledAsks

-- Overlay the dot on the above frame.
-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.
areaBetweenBidAndAsk :: [(V2 Double)] -> Picture
areaBetweenBidAndAsk = color (useColor lightpink) . Polygon . map v2ToPoint
