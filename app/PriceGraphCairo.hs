{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module PriceGraphCairo
  (priceGraph)
  where

import Graphics.Rendering.Cairo        hiding (scale, x, y)
import Data.Colour.Names
import Linear.V2
--
import ScaleCairo
import Types hiding (Bid,Ask)

type Bid = (Int,Double)

type Ask = (Int,Double)

priceGraph
  :: (Scale xscale
     ,Scale yscale)
  => xscale -> yscale -> [Bid] -> [Ask] -> Render ()
priceGraph xScale yScale bids asks =
    areaBetweenBidAndAsk areaVertices >>
     mapM_ dot scaledBids >>
     mapM_ dot scaledAsks
--      zip scaledAsks (repeat dot))
--      zip scaledAsks (repeat dot))
  where scaledBids = scaledPoints xScale yScale bids
        scaledAsks = scaledPoints xScale yScale asks
        areaVertices = scaledBids ++ reverse scaledAsks

-- Overlay the dot on the above frame.
-- Do not assume that the frame will be positioned at the
--  center. Explicitly position all the elements.
areaBetweenBidAndAsk :: [(V2 Double)] -> Render ()
areaBetweenBidAndAsk [] = return ()
areaBetweenBidAndAsk ((V2 ix iy):points) = do
  useColor lightpink
  l <- getLineJoin
  setLineJoin LineJoinRound
  moveTo ix iy
  mapM_ (\(V2 x y) -> lineTo x y) points
  closePath
  fill
  setLineJoin l
